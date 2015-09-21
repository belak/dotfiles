from i3pystatus import Status
from i3pystatus.updates import pacman, cower

status = Status(standalone=True, click_events=False)

status.register("clock", format="  %d-%m-%Y %H:%M", interval=20)

status.register("network",
                interface="wlp6s0",
                format_up="  {essid}",
                format_down="  none")

status.register("mem",
                format="  {percent_used_mem}%")

status.register("battery",
                format="{status} {percentage:.0f}% {remaining}",
                status={
                    'DPL':  '',
                    'FULL': '',
                    'CHR':  '',
                    'DIS':  ''
                },
                no_text_full=True)

status.register("pulseaudio",
                format="  {volume}",
                format_muted="  {volume}")

status.register("updates",
                format="  {count}",
                backends=[pacman.Pacman(), cower.Cower()])

status.run()
