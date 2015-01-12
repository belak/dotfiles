package main

import (
	"github.com/BurntSushi/xgb"
	"github.com/BurntSushi/xgb/xproto"
	"github.com/cloudfoundry/gosigar"

	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
	"unsafe"
)

// #cgo pkg-config: x11 xft
// #include <X11/Xft/Xft.h>
import "C"

type DesktopState struct {
	Occupied bool
	Selected bool
}

type Element struct {
	Pos  int
	Data string
}

type ByPos []Element

func (e ByPos) Len() int {
	return len(e)
}

func (e ByPos) Swap(i, j int) {
	e[i], e[j] = e[j], e[i]
}

func (e ByPos) Less(i, j int) bool {
	return e[i].Pos < e[j].Pos
}

type DesktopStateSlice []DesktopState

type BarState struct {
	Title    string
	Desktops []DesktopState

	BatCharging bool
	BatPercent  int

	Ssid string

	Temp       int
	CpuPercent int
}

type BarFont struct {
	Display *C.Display
	Font    *C.XftFont
}

var state BarState
var font BarFont
var refresh chan bool
var wg sync.WaitGroup
var width int

var fontName = "terminus:pixelsize=12"

var iconDir string

func init() {
	refresh = make(chan bool)
}

func (d DesktopState) String() string {
	var data string

	if d.Occupied {
		data = icon("full")
	} else {
		data = icon("empty")
	}

	if d.Selected {
		return fmt.Sprintf("^fg(white)%s^fg()", data)
	}

	return data
}

func (f BarFont) textWidth(text string) int {
	var extents C.XGlyphInfo
	str := (*C.FcChar8)(unsafe.Pointer(C.CString(text)))

	C.XftTextExtentsUtf8(f.Display, f.Font, str, C.int(len(text)), &extents)

	C.free(unsafe.Pointer(str))
	return int(extents.width)
}

func Ssid(data string) {
	state.Ssid = data
}

func Xtitle(data string) {
	state.Title = data
}

func WrapCommand(parser func(string), name string, args ...string) {
	defer wg.Done()

	cmd := exec.Command(name, args...)

	pipe, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Println(name+":", err)
		os.Exit(1)
	}

	out := bufio.NewScanner(pipe)
	cmd.Start()

	for out.Scan() {
		parser(out.Text())
		refresh <- true
	}

	err = cmd.Wait()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func Time() {
	defer wg.Done()

	ticker := time.NewTicker(time.Second * 1)

	// Last poll
	lastCpu := sigar.Cpu{}
	cpu := sigar.Cpu{}

	for {
		select {
		case <-ticker.C:
			// CPU info
			lastCpu = cpu
			cpu.Get()

			// TODO: Not sure which this actually handles
			// Sort of based off of this:
			//  http://stackoverflow.com/questions/22429036/obtain-cpu-usage-is-this-script-correct
			state.CpuPercent = int(100 - 100*(cpu.Idle-lastCpu.Idle)/(cpu.Total()-lastCpu.Total()))

			// Refresh the timer
			refresh <- true
		}
	}

	ticker.Stop()
}

func Battery(data string) {
	bat := strings.Split(data, " ")
	if len(bat) < 2 {
		return
	}

	state.BatCharging = bat[0] != "Discharging"
	state.BatPercent, _ = strconv.Atoi(bat[1])
}

func Bspwm(data string) {
	desktops := []DesktopState{}
	desktopData := strings.Split(data, ":")
	if len(desktopData) < 3 {
		return
	}

	for _, v := range desktopData[1 : len(desktopData)-1] {
		var occupied bool
		var selected bool

		if len(v) == 0 {
			continue
		}

		switch v[0] {
		case 'O':
			occupied = true
			selected = true
		case 'F', 'U':
			occupied = false
			selected = true
		case 'o':
			occupied = true
			selected = false
		case 'f':
			occupied = false
			selected = false
		case 'u':
			// TODO: Urgent
			occupied = false
			selected = true
		}

		desktops = append(desktops, DesktopState{occupied, selected})
	}

	state.Desktops = desktops
	refresh <- true
}

func icon(name string) string {
	return fmt.Sprintf(
		"^i(%s)",
		filepath.Join(iconDir, name+".xbm"),
	)
}

func percentBar(val, width int) string {
	return fmt.Sprintf(
		"^fg(white)^r(%dx1)^fg(darkgrey)^r(%dx1)^fg()",
		val*width/100,
		width-val*width/100,
	)
}

func Bar() {
	defer wg.Done()

	cmd := exec.Command("dzen2", "-h", "14", "-fn", fontName)
	pipe, err := cmd.StdinPipe()
	if err != nil {
		os.Exit(1)
	}

	in := bufio.NewWriter(pipe)
	err = cmd.Start()
	if err != nil {
		fmt.Println("dzen2:", err)
	}

	for {
		select {
		case <-refresh:
			buf := bytes.NewBufferString("")

			var elements ByPos

			// Left
			elements = append(
				elements,
				Element{16, state.Title},
			)

			// Middle
			// NOTE: Middle start point is:
			//  middle of screen - (desktop count * (icon width + padding) - padding)
			pos := width/2 - (len(state.Desktops)*20-12)/2
			for _, v := range state.Desktops {
				elements = append(
					elements,
					Element{pos, v.String()},
				)
				pos += 20
			}

			// Time
			timeStr := time.Now().Format("15:04")

			// width - txt width - 8 - 5
			// width - txt width - icon width - padding
			pos = width - font.textWidth(timeStr) - 13 - 16
			elements = append(
				elements,
				Element{
					pos,
					fmt.Sprintf("%s^p(5)%s", icon("clock"), timeStr),
				},
			)

			// Bat bar icon
			var iconName string
			if state.BatCharging {
				iconName = "ac"
			} else if state.BatPercent < 15 {
				iconName = "bat_empty_02"
			} else if state.BatPercent < 30 {
				iconName = "bat_low_02"
			} else {
				iconName = "bat_full_02"
			}

			// 8 + 5 + 50 + 10
			// icon width + 5px + bar + 10px
			pos -= (55 + 18)
			elements = append(
				elements,
				Element{
					pos,
					fmt.Sprintf(
						"%s^p(5)%s",
						icon(iconName),
						percentBar(state.BatPercent, 50),
					),
				},
			)

			// 8 + 5 + 50 + 10
			// icon width + 5px + bar + 10px
			pos -= (55 + 18)
			elements = append(
				elements,
				Element{
					pos,
					fmt.Sprintf(
						"%s^p(5)%s",
						icon("cpu"),
						percentBar(state.CpuPercent, 50),
					),
				},
			)

			// 8 + 5 + txt width + 10
			// icon width + padding + txt width + padding
			pos -= (8 + 5 + font.textWidth(strconv.Itoa(state.Temp)+"°") + 10)
			elements = append(
				elements,
				Element{
					pos,
					fmt.Sprintf(
						"%s^p(5)%d°",
						icon("temp"),
						state.Temp,
					),
				},
			)

			// 8 + 5 + txt width + 10
			// icon width + padding + txt width + padding
			/*pos -= (8 + 5 + font.textWidth(state.Ssid) + 10)
			elements = append(
				elements,
				Element{
					pos,
					fmt.Sprintf("%s^p(5)%s", icon("wifi_01"), state.Ssid),
				},
			)*/

			// Sort the elements and print them all
			sort.Sort(elements)
			for _, v := range elements {
				buf.WriteString(fmt.Sprintf("^pa(%d)%s", v.Pos, v.Data))
			}

			// I have no idea why, but moving to the end fixes some alignment issues
			buf.WriteString(fmt.Sprintf("^pa(%d)\n", width))
			//data := buf.String()
			//fmt.Println(data)
			//in.WriteString(data)
			in.Write(buf.Bytes())
			in.Flush()

			//fmt.Println(state)
			//fmt.Println(buf.String())
		}
	}
}

func main() {
	// Get the home dir for icon path
	home := os.Getenv("HOME")
	if home == "" {
		u, err := user.Current()
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		home = u.HomeDir
	}

	// Save the icon path
	iconDir = filepath.Join(home, ".belak", "icons", "8x8")

	// Open X display
	font.Display = C.XOpenDisplay(nil)
	if font.Display == nil {
		fmt.Println("Could not open the display")
		os.Exit(1)
	}
	defer C.XCloseDisplay(font.Display)

	// Get the screen width
	// This is in another scope because we don't need it any more
	{
		conn, err := xgb.NewConnDisplay("")
		if err != nil {
			fmt.Println("Could not connect to X")
			os.Exit(1)
		}
		setup := xproto.Setup(conn)
		screen := setup.DefaultScreen(conn)
		width = int(screen.WidthInPixels)
		conn.Close()
	}

	// Init font
	fontName := C.CString(fontName)
	font.Font = C.XftFontOpenXlfd(font.Display, 0, fontName)
	if font.Font == nil {
		font.Font = C.XftFontOpenName(font.Display, 0, fontName)
	}
	C.free(unsafe.Pointer(fontName))
	if font.Font == nil {
		fmt.Println("Could not open the font")
		os.Exit(1)
	}
	defer C.XftFontClose(font.Display, font.Font)

	wg.Add(5)

	go WrapCommand(Xtitle, "xtitle", "-s")
	go WrapCommand(Bspwm, "bspc", "control", "--subscribe")
	go WrapCommand(Battery, "battery", "-s", "-n", "0")
	//go WrapCommand(Ssid, "essid", "-s", "-w", "wlp3s0")
	go Time()

	go Bar()

	wg.Wait()
}
