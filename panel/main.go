package main

import (
	"github.com/BurntSushi/xgb"
	"github.com/BurntSushi/xgb/xproto"

	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
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

type DesktopStateSlice []DesktopState

type BarState struct {
	Title    string
	Desktops []DesktopState

	BatCharging bool
	BatPercent  int

	Ssid string
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
		data = fmt.Sprintf("^i(%s)", filepath.Join(iconDir, "full.xbm"))
	} else {
		data = fmt.Sprintf("^i(%s)", filepath.Join(iconDir, "empty.xbm"))
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
		fmt.Println(err)
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

	ticker := time.NewTicker(time.Second * 10)
	for {
		select {
		case <-ticker.C:
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

func Bar() {
	defer wg.Done()

	cmd := exec.Command("dzen2", "-h", "14", "-fn", fontName)
	pipe, err := cmd.StdinPipe()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	in := bufio.NewWriter(pipe)
	cmd.Start()

	for {
		select {
		case <-refresh:
			buf := bytes.NewBufferString("")

			buf.WriteString("^pa(4)")

			// Left
			buf.WriteString(state.Title)

			// Middle
			// NOTE: Middle start point is:
			//  middle of screen - (desktop count * (icon width + padding) - padding)
			pos := width/2 - (len(state.Desktops)*20-12)/2
			for _, v := range state.Desktops {
				buf.WriteString(fmt.Sprintf("^pa(%d)%s", pos, v))
				pos += 20
			}

			// Right
			timeStr := time.Now().Format("15:04")
			pos = width - font.textWidth(timeStr)

			buf.WriteString(
				fmt.Sprintf(
					"^pa(%d)^i(%s)^p(5)%s",
					pos-60-font.textWidth(state.Ssid)-18-18-18,
					filepath.Join(iconDir, "wifi_01.xbm"),
					state.Ssid,
				))

			// Bat bar
			var icon string
			if state.BatCharging {
				icon = filepath.Join(iconDir, "ac.xbm")
			} else if state.BatPercent < 15 {
				icon = filepath.Join(iconDir, "bat_empty_02.xbm")
			} else if state.BatPercent < 30 {
				icon = filepath.Join(iconDir, "bat_low_02.xbm")
			} else {
				icon = filepath.Join(iconDir, "bat_full_02.xbm")
			}

			buf.WriteString(
				fmt.Sprintf(
					"^pa(%d)^i(%s)^p(5)^fg(white)^r(%dx1)^fg(darkgrey)^r(%dx1)^fg()",
					pos-55-18-18,
					icon,
					state.BatPercent*50/100,
					50-state.BatPercent*50/100,
				))

			buf.WriteString(
				fmt.Sprintf(
					"^pa(%d)^i(%s)^p(5)%s",
					pos-18,
					filepath.Join(iconDir, "clock.xbm"),
					timeStr,
				))

			// End
			buf.WriteString("\n")
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

	wg.Add(6)

	go WrapCommand(Xtitle, "xtitle", "-s")
	go WrapCommand(Bspwm, "bspc", "control", "--subscribe")
	go WrapCommand(Battery, "battery", "-s")
	go WrapCommand(Ssid, "essid", "-s", "-w", "wlp3s0")
	go Time()

	go Bar()

	wg.Wait()
}
