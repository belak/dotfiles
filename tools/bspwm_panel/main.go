package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

const (
	// Any important colors and icons should be defined here so
	// they're easy to change later.
	successColor    = "%{F#8c9440}"
	warningColor    = "%{F#de935f}"
	errorColor      = "%{F#a54242}"
	foregroundColor = "%{F#707880}"
	selectedColor   = "%{F#c5c8c6}"

	clockIcon = "\ue015"

	batteryChargingIcon    = "\ue201"
	batteryFullIcon        = "\ue1ff"
	batteryDischargingIcon = "\ue1fe"
	batteryEmptyIcon       = "\ue1fd"

	occupiedIcon   = "\ue1c2"
	unoccupiedIcon = "\ue1bc"
)

var (
	// Internal state
	title    string
	curTime  string
	batState string
	wmState  string

	// Start the commands we're getting output from.
	titleChan = linesFromCommand("xtitle", "-s")
	clockChan = linesFromCommand("clock", "-s", "-f", "%H:%M %m-%d %a")
	batChan   = linesFromCommand("battery", "-s")
	bspwmChan = linesFromCommand("bspc", "subscribe")

	// Specify our fonts. These are essentially a main font and a
	// fallback font used for icons. This is an array of args and not
	// just the font names because it makes the code below simpler.
	lemonbarArgs = []string{
		"-F", foregroundColor,
		"-f", "-*-terminus-medium-r-normal-*-12-*-*-*-*-*-*-*",
		"-f", "-*-Siji-Medium-R-Normal--10-100-75-75-C-80-*-*",
	}
)

// Pretty much anything that fails should panic, so we have this
// helper function to save LoC.
func panicIfErr(err error) {
	if err != nil {
		panic(err.Error())
	}
}

// linesFromCommand will take a command and run it, sending each line
// as a message on the returned channel.
func linesFromCommand(executable string, args ...string) chan string {
	outputChan := make(chan string)

	go func() {
		cmd := exec.Command(executable, args...)
		stdout, err := cmd.StdoutPipe()
		panicIfErr(err)

		err = cmd.Start()
		panicIfErr(err)

		buf := bufio.NewReader(stdout)
		for {
			data, _, err := buf.ReadLine()
			panicIfErr(err)
			outputChan <- string(data)
		}
	}()

	return outputChan
}

func main() {
	barCmd := exec.Command("lemonbar", lemonbarArgs...)
	barStdout, err := barCmd.StdinPipe()
	panicIfErr(err)
	barCmd.Start()

	for {
		// Display the state we know about. This is split into
		// multiple writes for readability. Left, center, and right
		// are all on separate lines.
		fmt.Fprintf(barStdout, "%%{l} %s", title)
		fmt.Fprintf(barStdout, "%%{c}%s", wmState)
		fmt.Fprintf(barStdout, "%%{r}%s | %s ", batState, curTime)
		fmt.Fprintln(barStdout)

		// Get the next state update and handle it
		select {
		case line := <-titleChan:
			title = strings.Replace(line, "%", "%%", -1)
		case line := <-clockChan:
			curTime = clockIcon + " " + strings.Replace(line, "%", "%%", -1)
		case line := <-batChan:
			fields := strings.SplitN(line, " ", 2)
			if len(fields) < 2 {
				panic(fmt.Sprintf("Unexpected output from battery: %s (%d)", fields, len(fields)))
			}

			percent, err := strconv.Atoi(fields[1])
			panicIfErr(err)

			var icon, color string
			if fields[0] != "Discharging" {
				icon = batteryChargingIcon

				color = successColor
			} else {
				icon = batteryFullIcon

				if percent < 15 {
					color = errorColor
					icon = batteryEmptyIcon
				} else if percent < 30 {
					color = warningColor
				} else if percent < 60 {
					icon = batteryDischargingIcon
				}
			}

			batState = fmt.Sprintf("%s%s %d%%%%{F-}", color, icon, percent)
		case line := <-bspwmChan:
			out := bytes.NewBuffer(nil)

			// Give a font hint, so lemonbar doesn't have to guess.
			out.WriteString("%{T2}")

			for _, field := range strings.Split(line, ":") {
				if len(field) == 0 {
					continue
				}

				first, _ := utf8.DecodeRuneInString(field)

				// If it's upper case, we know it's selected,
				// otherwise it's not.
				if unicode.IsUpper(first) {
					out.WriteString(selectedColor)
				} else {
					out.WriteString(foregroundColor)
				}

				// 'f' is the only "empty" type.
				switch unicode.ToLower(first) {
				case 'f':
					out.WriteString(unoccupiedIcon)
				case 'o', 'u':
					out.WriteString(occupiedIcon)
				}
			}

			// Reset the color and font at the end.
			out.WriteString("%{F-}%{T-}")

			wmState = out.String()
		}
	}
}
