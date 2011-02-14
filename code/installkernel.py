#!/usr/bin/env python2
"""
Install the current kernel, from anywhere in the kernel source tree.
"""
import os, sys, argparse, subprocess, tempfile, pwd, grp, shutil

def goto_ksrc_root():
    """
    Change directory to the root of the kernel source.  We check all
    directories above us for that root.
    """
    # these are files that should be in the root kernel directory.
    # we look for these files to makes sure we are in the source root.
    rootfiles = ["Kbuild", "Makefile", "COPYING", "README", "CREDITS", "MAINTAINERS", "REPORTING-BUGS"] 

    for f in rootfiles:
        if not os.path.exists(f):
            os.chdir("..")
            if os.getcwd() == '/':
                sys.stderr.write(
                        "ERROR: we couldn't find the root of the kernel sources in any directory above us.\n")
                sys.exit(1)

def debug_print_cur_ids():
    "Print the current uids and gids. Create a temp file and show the owner of that file."
    print("getegid() = %s, geteuid() = %s, getgid() = %s, getuid() = %s" %
            (os.getegid(), os.geteuid(), os.getgid(), os.getuid()))
    print("getresgid() = %s, getresuid() = %s" % (os.getresgid(), os.getresuid()))
    print("getlogin() = %s" % (os.getlogin()))
    fd, name = tempfile.mkstemp(dir="/tmp")
    filestat = os.fstat(fd)
    uname = pwd.getpwuid(filestat.st_uid).pw_name
    gname = grp.getgrgid(filestat.st_gid).gr_name
    print ("file stat: %s -- (%s, %s)\n" % (name, uname, gname))
    os.close(fd)
    os.remove(name)

def run_command(command, dropprivs=True, replaceusergroup=False):
    """
    Run `command`. Drop privs to user running sudo if dropprivs is True.
    Replace strings SUDOUSER and SUDOGROUP with the user running sudo
    if replaceusergroup is True.
    """
    pid = os.fork()
    if not pid:
        owner_uid = os.stat(".").st_uid     # owner of the kernel sources
        original_uid = os.getuid()
        sudo_uid_gid_exist = ("SUDO_UID" in os.environ and \
                "SUDO_GID" in os.environ)

        if replaceusergroup and not sudo_uid_gid_exist:
            sys.stderr.write("ERROR: trying to replace sudo user and group "
                    "in command `%s`, but this command is not being run "
                    "with SUDO_UID and SUDO_GID enviroment variables "
                    "available.\n" % command)
            sys.exit(1)

        if replaceusergroup:
            sudo_uid = int(os.environ["SUDO_UID"])
            sudo_gid = int(os.environ["SUDO_GID"])
            sudo_user_name = pwd.getpwuid(sudo_uid).pw_name
            sudo_group_name = grp.getgrgid(sudo_gid).gr_name
            command = command.replace("SUDOUSER", sudo_user_name)
            command = command.replace("SUDOGROUP", sudo_group_name)

        if sudo_uid_gid_exist and \
                (owner_uid != original_uid) and (dropprivs == True):
            # we are being run in sudo, so we need to drop privs to run make
            sudo_uid = int(os.environ["SUDO_UID"])
            sudo_gid = int(os.environ["SUDO_GID"])
            os.setgid(sudo_gid)
            os.setuid(sudo_uid)


        # need to get this 
        cur_user_name = pwd.getpwuid(os.getuid()).pw_name

        print("Running `%s`..." % command)
        retcode = subprocess.call(command, shell=True, stdout=sys.stdout, stderr=sys.stderr)
        if retcode < 0:
            sys.stderr.write("ERROR: `%s` was terminated by signal %s.\n" % 
                    (command, -retcode))
            sys.exit(1)
        elif retcode > 0:
            sys.stderr.write("ERROR: `%s` failed while "
                    "running as user %s.\n" % (command, cur_user_name))
            sys.exit(1)
        print "Finished `%s`.\n" % command
        sys.exit(0)
    else:
        waitpid, exit_status = os.wait()
        if exit_status != 0:
            sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description="Install the current kernel.")
    parser.add_argument('--make', '-m', action='store_true', 
            help="run `make` first")
    parser.add_argument('--make-install-modules', '-i', action='store_true', 
            help="run `make modules_install`")
    args = parser.parse_args()

    goto_ksrc_root()

    if args.make:
        run_command("make")

    if args.make_install_modules:
        run_command("make modules_install", dropprivs=False)
        run_command("chown -R SUDOUSER:SUDOGROUP .", dropprivs=False,
                replaceusergroup=True)


    # make sure you can write to boot
    if not os.access("/boot", os.W_OK):
        sys.stderr.write("ERROR: cannot write to /boot. Need to run this script with `sudo`?\n")
        sys.exit(1)

    # get the kernel release (somethingl like 2.6.34-MY-KERNEL)
    p = subprocess.Popen(["make", "kernelrelease"], stdout=subprocess.PIPE)
    release = p.communicate()[0].strip()

    # get the current arch
    p = subprocess.Popen(["uname", "-m"], stdout=subprocess.PIPE)
    arch = p.communicate()[0].strip()

    bzimage = "arch/%s/boot/bzImage" % arch
    bzimage_install_path = "/boot/bzImage-%s" % release

    config = ".config"
    config_install_path = "/boot/config-%s" % release

    systemmap = "System.map"
    systemmap_install_path = "/boot/System.map-%s" % release

    def install_file(file_name, install_path):
        "Install a file using shutil.copyfile() with file_name as first arg and install_path as second."
        try:
            print "Copying %s to %s" % (file_name, install_path)
            shutil.copyfile(file_name, install_path)
        except OSError, e:
            sys.stderr.write("ERROR: error when trying to copy %s to %s: %s\n" %
                    (file_name, install_path, e))
            sys.exit(1)

    install_file(bzimage, bzimage_install_path)
    install_file(config, config_install_path)
    install_file(systemmap, systemmap_install_path)


if __name__ == '__main__':
    main()
