#!/usr/bin/python2 -O

"""Mplayer Spot remembers the spot in which you last exited 
mplayer and resumes from that spot the next time you start 
watching the same movie."""

import sys
import os
import re
from subprocess import Popen, PIPE, STDOUT

### SOME config vars

# dot directory to save files in
mplayerspot_dot_dir = os.path.expanduser("~/.mplayer-spot")
spots_dir = os.path.join(mplayerspot_dot_dir, "spots")

# Smallest length of movie to save position in seconds.
# For instance, we will not save the position for files
# under 20 minutes.
minimum_save_length = 20 * 60 # 30 minutes

# Leftover length of movie to consider it fully watched in seconds.
# For example, we will consider a movie fully watched even if 
# we exit with 5 minutes left.
leftover_okay_length = 5 * 60 # 5 minutes


class read_mplayer_line:
    """This is just a class that produces an iterator for reading over
    lines of mplayer output.  A special class is needed for this because
    mplayer users ansi escape sequences to manipulate the cursor, and 
    it is taken care of in this method.  Those special sequences are
    just converted to newlines."""

    def __init__(self, mplayer_stdout):
        self.stdout = mplayer_stdout
        self.eof_reached = False

    def next(self):
        if self.eof_reached:
            raise StopIteration

        line = ''
        while True:
            read_char = self.stdout.read(1)
            # if we can't read anything then we have come to EOF
            if not read_char:
                self.eof_reached = True
                read_char = '\n'

            line += read_char
            #line = self._fix_cursor_moving_esc_seq(line)

            # we only continue to process if this is the end of a line
            if line[-4:] == '\x1b[J\x0d' or line[-1] == '\n':
                return line

    def _fix_cursor_moving_esc_seq(self, line):
        """Mplayer uses ansi escape sequences to move the cursor
        around.  This takes out thos escape sequences and just
        replaces them with a newline.  If there are no escape sequences,
        this just returns line."""
        if line[-4:] == '\x1b[J\x0d':
            return line[:-4] + '\n'
        else:
            return line

    def __iter__(self):
        return self

def get_filename(line):
    "Gets the filename of a movie file."
    if line.startswith("ID_FILENAME="):
        return line.split('=', 1)[1]
    else:
        return None

def get_length(line):
    "Gets the total length of the file."
    if line.startswith("ID_LENGTH="):
        return float(line.split('=')[1])

def is_close(allowed_difference, a, b):
    "Returns true if a and b are close together."
    return abs(a - b) <= allowed_difference

# regular expression to make sure we match the video and audio position
cur_pos_regex = re.compile("A: *(?P<audio_time>\d+\.\d+) V: *(?P<video_time>\d+\.\d+) A-V:")
def get_cur_pos(line):
    """Gets the current position in the movie file by using a regex
    (so that we can save the position and return to it later.)"""
    result = cur_pos_regex.match(line)

    if not result:
        return None

    audio_time = float(result.group("audio_time"))
    video_time = float(result.group("video_time"))
    assert is_close(1.0, audio_time, video_time)
    return video_time


def write_out_spot_files(spots_dir, filename, length, ending_pos):
    """For each file in filename[], writes out a file in spots_dir 
    named file with the correct ending_pos (as long as it should be kept)."""
    if (len(filename) != len(length) or len(length) != len(ending_pos)):
        return

    for i in range(len(filename)):
        save_path = os.path.join(spots_dir, os.path.basename(filename[i].strip()))

        if length[i] < minimum_save_length:
            print "not saving because length[i] (" + str(length[i]) + ") is too short."
            continue
        if is_close(leftover_okay_length, length[i], ending_pos[i]):
            print "not saving because ending_pos[i] (" + str(ending_pos[i]) + \
                    ") is close to length[i] (" + str(length[i]) + ")."
            if os.path.exists(save_path):
                print "deleting old save file because we have finished this file."
                os.remove(save_path)
            continue

        print "saving to path: " + save_path
        f = open(save_path, 'w')
        f.write(str(ending_pos[i]) + "\n")

def seek_to_correct_location(mplayer_stdin, spots_dir, filename):
        save_path = os.path.join(spots_dir, os.path.basename(filename.strip()))

        if os.path.exists(save_path):
            f = open(save_path, "r")
            leftoff = int(float(f.read().strip()))
            if leftoff > 21:
                leftoff -= 20
            leftoff_string = str(leftoff)
            print "writing to mplayer: seek " + leftoff_string + "\n"
            mplayer_stdin.write("seek " + leftoff_string + "\n")



def main():

    # try to open .mplayer-spot/ and .mplayer-spot/spots/.
    # If they doesn't exist, create them
    if not os.path.isdir(mplayerspot_dot_dir):
        os.mkdir(mplayerspot_dot_dir)
    if not os.path.isdir(spots_dir):
        os.mkdir(spots_dir)


    mplayer_process = Popen(["mplayer", "-identify", "-slave"] + sys.argv[1:], 
            stdout=PIPE, stderr=STDOUT, stdin=PIPE, bufsize=5)

    mplayer_stdout = mplayer_process.stdout
    mplayer_stderr = mplayer_process.stderr
    mplayer_stdin = mplayer_process.stdin

    cur_movie_index = -1
    filenames = []
    lengths = []
    ending_pos = []

    try:
        for line in read_mplayer_line(mplayer_stdout):
            sys.stdout.write(line)

            # if we read in a new filename, then we can update our arrays
            new_filename = get_filename(line)
            if new_filename != None:
                filenames.append(new_filename)
                seek_to_correct_location(mplayer_stdin, spots_dir, new_filename)
                lengths.append(None)
                ending_pos.append(None)
                cur_movie_index += 1

            if cur_movie_index < 0:
                continue

            if not lengths[cur_movie_index]:
                lengths[cur_movie_index] = get_length(line)

            tmp_cur_pos = get_cur_pos(line)
            if tmp_cur_pos:
                ending_pos[cur_movie_index] = tmp_cur_pos
    except KeyboardInterrupt:
        pass


    assert (len(filenames) == len(ending_pos) and len(ending_pos) == len(lengths))
    print "filenames are: " + str(filenames)
    print "Lengths are: " + str(lengths)
    print "exit positions are: " + str(ending_pos)

    if len(lengths) == 0 or len(ending_pos) == 0:
        print sys.argv[0] + ": ERROR! Could not play movie."
        sys.exit(1)

    write_out_spot_files(spots_dir, filenames, lengths, ending_pos)


if __name__ == '__main__':
    main()
