#!/usr/bin/env python3

import argparse
import re
import sys

from subprocess import Popen, PIPE

# setup debugging
def debug(arg = ""):
    print("*** -> %s" % arg)
def nodebug(arg = ""):
    pass
DEBUG = nodebug

# regex strings for checking which monitors are active.
# These may have to be changed if I get a new monitor or tv or something."
LCD_ACTIVE_REGEX = 'LVDS-1 connected (primary )?1920x1080\+0\+600 \(normal left inverted right x axis y axis\) 344mm x 193mm\n   1920x1080      60\.0\*\+   60\.0     50\.0'
EXTERNAL_MONITOR_ACTIVE_REGEX = 'DVI-I-1 connected (primary )?1080x1920\+1920\+0 left \(normal left inverted right x axis y axis\) 510mm x 287mm\n   1920x1080      60.0\*\+   50.0'
TV_ACTIVE_REGEX = 'HDMI-1 connected (primary )?1920x1080\+0\+600 \(normal left inverted right x axis y axis\) 886mm x 498mm\n   1920x1080      60.0\*\+   '

XRANDR_LCD_AND_EXTERN_MON_COMMAND = 'xrandr --fb 3000x1920 --output HDMI-1 --off && xrandr --fb 3000x1920 --output LVDS-1 --auto --primary --pos 0x600 --output DVI-I-1 --rotate left  --pos 1920x0 --mode "1920x1080"'
XRANDR_TV_AND_EXTERN_MON_COMMAND = 'xrandr --fb 3000x1920 --output LVDS-1 --off && xrandr --fb 3000x1920 --output HDMI-1 --auto --primary --pos 0x600 --output DVI-I-1 --rotate left  --pos 1920x0 --mode "1920x1080"'

def setup_lcd_and_extern_mon(dryrun):
    DEBUG("Using xrandr to setup LCD and external monitor with this command:")
    DEBUG("\t'%s'" % XRANDR_LCD_AND_EXTERN_MON_COMMAND)
    if dryrun:
        DEBUG("Not run because only dry-run.");
    else:
        Popen(XRANDR_LCD_AND_EXTERN_MON_COMMAND, shell=True)


def setup_tv_and_extern_mon(dryrun):
    DEBUG("Using xrandr to setup TV and external monitor with this command:")
    DEBUG("\t'%s'" % XRANDR_TV_AND_EXTERN_MON_COMMAND)
    if dryrun:
        DEBUG("Not run because only dry-run.");
    else:
        Popen(XRANDR_TV_AND_EXTERN_MON_COMMAND, shell=True)

def runcommand(args):
    """
    Run a command and check stderr.
    If anything is found on stderr, exit.
    Return stdout.
    """
    stdout, stderr = Popen(args, stdout=PIPE, stderr=PIPE).communicate()
    stdout = stdout.decode('utf-8')
    stderr = stderr.decode('utf-8')

    if stderr:
        DEBUG("ERROR when running command '%s':\n%s" % (args, stderr))
        sys.exit(1)

    return stdout

def main():
    global DEBUG

    parser = argparse.ArgumentParser(description="Install the current kernel.")
    parser.add_argument('--startup', '-s', action='store_true', default=False,
            help="setup LCD screen with external monitor")
    parser.add_argument('--debug', '-d', action='store_true', default=False,
            help="print debug messages")
    parser.add_argument('--dry-run', '-n', action='store_true', default=False,
            help="print debug messages")
    args = parser.parse_args()

    if args.debug:
        DEBUG = debug

    # get xrandr output
    out = runcommand("xrandr")
    DEBUG("xrandr output:\n%s" % out)

    # find out if tv is connected
    tv_avail = False
    if re.search("HDMI-1 connected", out):
        tv_avail = True
        DEBUG("found TV connected")
    else:
        DEBUG("did not find TV connected")

    # find out if external monitor is connected
    extern_mon_avail = False
    if re.search("DVI-I-1 connected", out):
        extern_mon_avail = True
        DEBUG("found external monitor connected")
    else:
        DEBUG("did not find external monitor connected")

    # find out if lcd is active
    lcd_active = False
    if re.search(LCD_ACTIVE_REGEX, out):
        lcd_active = True
        DEBUG("found lcd active")
    else:
        DEBUG("did not find lcd active")

    # find out if tv is active
    tv_active = False
    if re.search(TV_ACTIVE_REGEX, out):
        tv_active = True
        DEBUG("found tv active")
    else:
        DEBUG("did not find tv active")

    # find out if external monitor is active
    extern_mon_active = False
    if re.search(EXTERNAL_MONITOR_ACTIVE_REGEX, out):
        extern_mon_active = True
        DEBUG("found external monitor active")
    else:
        DEBUG("did not find external monitor active")

    # put a new line in the debugging output to make it easier to read
    DEBUG()

    if args.startup:
        if extern_mon_avail:
            setup_lcd_and_extern_mon(args.dry_run)
        else:
            DEBUG("called with --startup, but external monitor is not available, so not doing anything")
        sys.exit(0)

    if lcd_active == True and tv_avail == True and tv_active == False and extern_mon_avail == True and extern_mon_active == True:
        setup_tv_and_extern_mon(args.dry_run)
    elif lcd_active == False and tv_avail == True and tv_active == True and extern_mon_avail == True and extern_mon_active == True:
        setup_lcd_and_extern_mon(args.dry_run)
    elif extern_mon_avail == True:
        DEBUG("Monitors are in weird state, but external monitor is available, ")
        DEBUG("so we will just use the external monitor and reset to the default state. ")
        DEBUG("This is exactly what would happen if you were to run with --startup.")
        DEBUG("Maybe try running again with --debug to figure out what's going on.")
        setup_lcd_and_extern_mon(args.dry_run)
    else:
        DEBUG("Monitors are in weird state, so not doing aything.")
        DEBUG("Try running with --startup flag or --debug flags.")



if __name__ == '__main__':
    main()
