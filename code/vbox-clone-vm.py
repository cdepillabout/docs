#!/usr/bin/env python

import argparse
import os
import re
import sys

from subprocess import Popen, PIPE

class VM:
    """
    An object that represents a VirtualBox VM.
    """
    def __init__(self, line, hddforest):
        self.hddforest = hddforest

        m = re.match(r'^"(.+?)" {([\w\d-]+)}$', line)
        assert(len(m.groups()) == 2)
        self.name = m.group(1)
        self.uuid = m.group(2)
        self.info = None

    def __str__(self):
        return "VM(name: %s, uuid: %s)" % (self.name, self.uuid)

    def __repr__(self):
        return self.__str__()

    def fillininfo(self):
        stdout = runcommand(["VBoxManage", "showvminfo", self.uuid, "--machinereadable"])
        lines = stdout.strip().split('\n')

        def create_values(line):
            def take_out_quotes(string):
                if len(string) <= 2:
                    return string
                if string[0] == '"':
                    string = string[1:]
                if string[-1] == '"':
                    string = string[:-1]
                return string

            key, value = line.strip().split('=')
            return take_out_quotes(key), take_out_quotes(value)

        self.info = {}
        for line in lines:
            key, value = create_values(line)

            # this is a hack because the value of firmware ("BIOS") is
            # capitalized when it should not be.
            if key.lower() == "firmware":
                value = value.lower()

            self.info[key.lower()] = value

    def __setoption(self, fromvm, option):
        """
        Set an option from another vm.  fromvm must have a dictionary
        "info" that has its options and values.
        
        Returns True if option was set, and False if not.
        """
        if option in fromvm.info.keys():
            runcommand(["VBoxManage", "modifyvm", self.uuid,
                "--%s" % option, "%s" % fromvm.info[option]])
            #print("Setting option --%s to \"%s\"" % (option, fromvm.info[option]))
            return True
        else:
            #print("Not setting option --%s because other vm does not have it set." % option)
            return False

    def __setstoragecontrolleroption(self, fromvm, name, stype, bootable):
        """
        Set a storage option from another vm.  fromvm must have a dictionary
        "info" that has its options and values.
        
        Returns True if option was set, and False if not.
        """
        vmoptions = fromvm.info.keys()
        if name in vmoptions and stype in vmoptions:
            cmdline = ["VBoxManage", "storagectl", self.uuid,
                "--name", fromvm.info[name], "--controller", fromvm.info[stype]]

            if bootable in vmoptions:
                cmdline.append("--bootable")
                cmdline.append(fromvm.info[bootable])

            contype = fromvm.info[stype]
            cmdline.append("--add")
            if contype in ["PIIX4", "PIIX3", "ICH6"]:
                cmdline.append("ide")
            elif contype in ["I82078"]:
                cmdline.append("floppy")
            elif contype in ["IntelAhci"]:
                cmdline.append("sata")
            elif contype in ["LsiLogic", "BusLogic"]:
                cmdline.append("scsi")
            elif contype in ["LSILogicSAS", "BusLogic"]:
                cmdline.append("sas")
            elif contype in ["unknown"]:
                #print("Not setting storage controller because type is unknown.")
                return True
            else:
                print("ERROR! Could not figure out controller type.")
                sys.exit(1)

            runcommand(cmdline)
            #print("Setting storrage controller option.")
            return True
        else:
            #print("Not setting storage controller because other vm does not have it set.")
            return False

    def __setstoragedevices(self, fromvm):
        """
        Set the storage devices from the new vm from the old vm, 
        cloning them if necessary.
        """
        cloned_hdds = 1
        # get all the storage controller name and type options
        nameopts = [opt for opt in fromvm.info.keys() if opt.startswith("storagecontrollername")]
        typeopts = [opt for opt in fromvm.info.keys() if opt.startswith("storagecontrollertype")]
        nameopts.sort()
        typeopts.sort()
        allopts = zip(nameopts, typeopts)
        #print()
        for nameopt, typeopt in allopts:
            name = fromvm.info[nameopt]
            taip = fromvm.info[typeopt]
            if taip == "unknown":
                # we don't know what do to with unknown devices
                print("Skipping unknown device...")
                continue
            #print("name: %s (%s), type: %s (%s)" % (name, nameopt, taip, typeopt))

            #controlleropts = [opt for opt in fromvm.info.keys() if opt.startswith(name.lower())]
            controlleropts = [opt for opt in fromvm.info.keys() if
                    re.match(r'^%s-\d\d?-\d\d?$' % name.lower(), opt)]
            for controlopt in controlleropts:
                if fromvm.info[controlopt] == "none":
                    # there is nothing here, just ignore it
                    #print("\t(none)")
                    continue

                # try to figure out whether this has an imageuuid varaiable associated with it
                m = re.match(r'^(.*?)-(\d\d?)-(\d\d?)$', controlopt)
                assert(m)
                tmpname = m.group(1)
                port = m.group(2)
                device = m.group(3)
                imageuuidopt = "%s-imageuuid-%s-%s" % (tmpname, port, device)
                imageuuid = None
                if imageuuidopt in fromvm.info:
                    imageuuid = fromvm.info[imageuuidopt]

                if fromvm.info[controlopt] == "emptydrive":
                    # attach empty drive
                    #print("\tAttaching empty device to %s... " % name)
                    cmdline = ["VBoxManage", "storageattach", self.uuid,
                        "--storagectl", name,
                        "--port", port,
                        "--device", device,
                        "--medium", "emptydrive"]
                    runcommand(cmdline)
                    continue

                #print("\t%s: %s" % (controlopt, fromvm.info[controlopt]))
                #print("\timageuuid: %s" % imageuuid)
                assert(imageuuid)

                strgtype = storagetype(imageuuid)
                #print("\tstorage type: %s" % strgtype)
                if strgtype in ["dvd", "floppy", "hostdvd", "hostfloppy"]:
                    #print("\tAttaching %s to %s... " % (strgtype, name))
                    cmdline = ["VBoxManage", "storageattach", self.uuid,
                        "--storagectl", name,
                        "--port", port,
                        "--device", device,]

                    cmdline.append("--medium")
                    if strgtype in ["hostdvd", "hostfloppy"]:
                        cmdline.append("host:%s" % imageuuid)
                    else:
                        cmdline.append("%s" % imageuuid)

                    cmdline.append("--type")
                    if strgtype in ["dvd", "hostdvd"]:
                        cmdline.append("dvddrive")
                    if strgtype in ["floppy", "hostfloppy"]:
                        cmdline.append("floppy")

                    runcommand(cmdline)
                    continue

                # it wasn't an empty drive, or a dvd/floppy drive, so it must be a hard drive
                assert(strgtype == "hdd")
                #print("fromvm: %s" % fromvm)
                #print("hddforest: %s" % self.hddforest)
                hdds = hddsattachedto(fromvm.uuid, self.hddforest)
                #print("hdds: %s" % hdds)

                # get the hdd whose uuid matches imageuuid
                tmphdds = [hdd for hdd in hdds if hdd.uuid == imageuuid]
                assert(len(tmphdds) == 1)
                hdd = tmphdds[0]
                #print("hdd: %s" % hdd)
                self.fillininfo()
                #print("self info: %s" % self.info)
                # just look for the config file and assume we 
                # can throw the hdd in the same dir
                configfile = self.info["cfgfile"]
                assert(os.path.isfile(configfile))
                dirname = os.path.dirname(configfile)

                newlocation = os.path.join(dirname, "%s-%s.vdi" % (self.name, cloned_hdds))
                cmdline = ["VBoxManage", "clonehd", hdd.uuid, newlocation]
                #print("cmdline: %s" % cmdline)
                stdout, stderr = Popen(cmdline, stdout=PIPE,
                        stderr=PIPE).communicate()
                stdout = stdout.decode('utf-8')
                stderr = stderr.decode('utf-8')

                if re.search("error", stderr, re.I):
                    print("ERROR! Could not run command %s:\n%s" % (cmdline, stderr))
                    sys.exit(1)

                cloned_hdds += 1

                newhddforest = createHDDForest()
                tmphdds = [hdd for hdd in newhddforest.values() if hdd.hdlocation == newlocation]
                assert(len(tmphdds) == 1)
                newhdd = tmphdds[0]
                #print("new hdd: %s" % [hdd for hdd in newhddforest.values() if hdd.hdlocation == newlocation])

                #print("Attaching new hard drive %s..." % newhdd.uuid)
                cmdline = ["VBoxManage", "storageattach", self.uuid,
                    "--storagectl", name,
                    "--port", port,
                    "--device", device,
                    "--medium", newhdd.uuid,
                    "--type", "hdd"]
                runcommand(cmdline)

    def setinfofrom(self, fromvm):
        "Copy the info from the other vm to this vm."
        options_to_copy = [
                "accelerate3d",
                "acpi",
                "audio",
                "boot1",
                "boot2",
                "boot3",
                "boot4",
                "clipboard",
                "cpus",
                "firmware",
                "guestmemoryballoon",
                "hpet"
                "hwvirtex",
                "hwvirtexexl",
                "ioacpi",
                "largepages",
                "memory",
                "monitorcount",
                "nextedpaging",
                "pae",
                "rtcseutc",
                "usb",
                "usbehci",
                "vram",
                "vrdeaddress",
                "vrdeauthtype",
                "vrdeauthtype",
                "vrdemulticon",
                "vrdeport",
                "vrdereusecon",
                "vrdevideochannel",
                "vrdevideochannelquality",
                "vtxvpid",
                ]

        sys.stdout.write("Setting options for new VM from old VM (this may take a while)... ")
        sys.stdout.flush()

        for option in options_to_copy:
            self.__setoption(fromvm, option)

        print("Done.")
        sys.stdout.write("Setting network options for new VM from old VM... ")
        sys.stdout.flush()

        multi_options_to_copy = [
                "nic",
                "nictype",
                "cableconnected",
                "bridgeadapter",
                "hostonlyadapter",
                "intnet",
                "vdenet",
                ]

        # continue trying to set options until we get to a round where no options are set
        i = 1
        while True:
            did_set_list = []
            for option in multi_options_to_copy:
                did_set_list.append(self.__setoption(fromvm, "%s%d" % (option, i)))
            if not any(did_set_list):
                break
            i += 1

        print("Done.")
        sys.stdout.write("Setting storage controller options for new VM from old VM... ")
        sys.stdout.flush()

        i = 0
        did_set_option = True
        while did_set_option:
            did_set_option = self.__setstoragecontrolleroption(fromvm,
                    "storagecontrollername%d" %  i,
                    "storagecontrollertype%d" %  i,
                    "storagecontrollerbootable%d" %  i)
            i += 1

        print("Done.")
        sys.stdout.write("Copying storage devices for new VM from old VM (this may take a long time)... ")
        sys.stdout.flush()

        self.__setstoragedevices(fromvm)

        print("Done.")

class HDD:
    """
    Create a vdi entry from the output of `VBoxManage list hdds`.
    """
    def __init__(self, lines, forest=None, parent=None):
        """
        Initialize an hdd.  lines is a list of the 7 lines of output
        from `VBoxManage list hdds`.  forest is a Forest object or None.
        parent is the parent HDD object or None.
        """
        assert(len(lines) >= 6)

        self.uuid = re.sub(r'^UUID:\W+', '', lines[0])
        self.parentuuid = re.sub(r'^Parent UUID:\W+', '', lines[1])
        self.hdformat = re.sub(r'^Format:\W+', '', lines[2])
        self.hdlocation = re.sub(r'^Location:\s+', '', lines[3])
        self.hdstate = re.sub(r'^State:\W+', '', lines[4])
        self.hdtype = re.sub(r'^Type:\W+', '', lines[5])

        self.hdusage = None
        self.hdvm = None
        self.hdvmuuid = None
        self.hdsnapshot = None
        self.hdsnapshotuuid = None
        if len(lines) == 7:
            self.hdusage = re.sub(r'^Usage:\W+', '', lines[6])

            m = re.match(r'^(.*?) \(UUID: ([\w\d-]+)\)$', self.hdusage)
            if not m:
                m = re.match(r'^(.*?) \(UUID: ([\w\d-]+)\) \[(.*?) \(UUID: ([\w\d-]+)\)\]$', self.hdusage)
                if not m:
                    print("Couldn't get usage information for hd %s: %s" % (self.uuid, self.hdusage))
                    sys.exit(1)

            assert(len(m.groups()) >= 2)
            self.hdvm = m.group(1)
            self.hdvmuuid = m.group(2)
            if len(m.groups()) == 4:
                self.hdsnapshot = m.group(3)
                self.hdsnapshotuuid = m.group(4)

        self.forest = forest
        self.parent = parent

    def __str__(self):
        string = ""
        string += "uuid: %s\n" % self.uuid
        string += "parent: %s\n" % self.parentuuid
        string += "format: %s\n" % self.hdformat
        string += "location: %s\n" % self.hdlocation
        string += "state: %s\n" % self.hdstate
        string += "type: %s\n" % self.hdtype
        if self.hdvm:
            #string += "usage: %s\n" % self.hdusage
            string += "vm: %s (%s)\n" % (self.hdvmuuid, self.hdvm)
            if self.hdsnapshot:
                string += "snapshot: %s (%s)\n" % (self.hdsnapshotuuid, self.hdsnapshot)

        if self.parent:
            string += "(parent: %s)\n" % self.parent.uuid
        if self.forest and self.forest.getChildren(self.uuid):
            string += "(children: %s)\n" % [child.uuid for child in self.forest.getChildren(self.uuid)]
        return string

    def __repr__(self):
        return self.__str__()

class Forest:
    """
    Forest of multiple trees of unique nodes.
    Each node needs to have a uuid and parent member."""

    def __init__(self):
        self.nodes = {}

    def __getitem__(self, key):
        return self.nodes[key]

    def __setitem__(self, key, new_node):
        assert(key == new_node.uuid)
        for n in self.nodes.values():
            if n.parentuuid == new_node.uuid:
                n.parent = new_node
            if n.uuid == new_node.parentuuid:
                new_node.parent = n
        self.nodes[key] = new_node

    def __contains__(self, key):
        return self.nodes.__contains__(key)

    def __iter__(self):
        return self.nodes.__iter__()

    def __len__(self):
        return len(self.nodes)

    def __str__(self):
        string = "%s nodes\n" % len(self.nodes)
        for node in self.nodes.values():
            string += "%s\n" % node

        return string

    def items(self):
        return self.nodes.items()

    def values(self):
        return self.nodes.values()

    def getends(self):
        "Return a list of nodes in forest that have no children."
        return [n for n in self.nodes.values() if not self.getChildren(n.uuid)]

    def getChildren(self, parent_node_uuid):
        """
        Return list of children for the parent node. parent_node_uuid is
        just a string of the parent node's uuid.
        """
        assert(type(parent_node_uuid) != type(HDD))
        return [n for n in self.nodes.values() if n.parentuuid == parent_node_uuid]

def storagetype(devuuid):
    """
    Return the storage type for the device with uuid devuuid.
    If the storage type is a host dvd, return the string "hostdvd".
    If the storage type is a host floppy, return the string "hostfloppy".
    If the storage type is a floppy, return the string "floppy".
    If the storage type is a dvd, return the string "dvd".
    If the storage type is a hdd, return the string "hdd".
    """
    listcommands = [
            ("hostdvds", "hostdvd"),
            ("hostfloppies", "hostfloppy"),
            ("floppies", "floppy"),
            ("dvds", "dvd"),
            ("hdds", "hdd"),
            ]

    for c, ret in listcommands:
        stdout = runcommand(["VBoxManage", "list", c])
        lines = stdout.strip().split('\n')
        for l in lines:
            m = re.match(r'^UUID:\s+%s$' % devuuid, l)
            if m:
                return ret
            else:
                continue

    return None

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
        print("ERROR! Could not run command %s:\n%s" % (args, stderr))
        sys.exit(1)

    warning = checkWarning(stdout)
    if warning:
        print(warning)
        sys.exit(1)

    return stdout

def createNewVM(name, ostype, hddforest):
    "Create a new VM with name and ostype.  Return new vm."

    sys.stdout.write("Creating new vm... ")
    runcommand(["VBoxManage", "createvm", "--name", name, "--register", "--ostype", ostype])
    print("Done.")
    return getVM(hddforest, name)

def checkWarning(stdout):
    """
    Checks the output of VBoxManage and makes sure there is no warning.
    Returns the warning string if there is a warning or None if no warning.
    """
    # is there a warning?
    m = re.match("^(WARNING: )", stdout)
    if not m:
        return None

    # return the warning text
    m = re.match("^(WARNING: )(.*?)(\nUUID:.*)?$", stdout, re.S)
    assert(len(m.groups()) >= 2)
    return m.group(1) + m.group(2)

def createHDDForest():
    """
    Return a Forest() object of all hdds available.
    This does not include snapshots.
    """
    stdout = runcommand(["VBoxManage", "list", "hdds"])
    stdout = stdout.strip()
    lines = stdout.split("\n\n")

    if len(lines) == 0:
        print("ERROR: No hdds.")
        sys.exit(1)

    forest = Forest()
    for l in lines:
        hdd = HDD(l.strip().split("\n"), forest)
        forest[hdd.uuid] = hdd

    return forest

def getVM(hddforest, vmname=None):
    """
    Get a vm, or a list of all vms.  If vmname is None, then we
    just get a list of all VMs.  If vmname is specified, then 
    we return just the named vm.
    
    This does not return snapshots.
    """
    stdout = runcommand(["VBoxManage", "list", "vms"])
    stdout = stdout.strip()
    lines = stdout.split('\n')
    vms = [VM(l, hddforest) for l in lines]
    if not vmname:
        return vms
    named_vms = [vm for vm in vms if vm.name == vmname or vm.uuid == vmname]
    assert(len(named_vms) == 1)
    return named_vms[0]

def hddsattachedto(vm, hddforest):
    "Return a list of all hdds attached to a vm with a uuid of vmuuid."
    assert(type(vm) == type(""))
    return [hdd for hdd in hddforest.getends() if hdd.hdvm == vm or hdd.hdvmuuid == vm]

def printVMs(vms):
    "Print a list of existing VMs."
    longest_vm = max([len(vm.name) for vm in vms])
    for vm in vms:
        print("%-*s  {%s}" % (longest_vm, vm.name,  vm.uuid))

def main():
    hddforest = createHDDForest()
    vms = getVM(hddforest)

    def checkvmtype(string):
        vms_with_this_name = [vm for vm in vms if vm.name == string or vm.uuid == string]
        if not vms_with_this_name:
            raise argparse.ArgumentTypeError("No vms with name \"%s\"." % string)
        if len(vms_with_this_name) > 1:
            raise argparse.ArgumentTypeError("Multiple vms with name \"%s\"." % string)
        return string

    parser = argparse.ArgumentParser(description="Clone the current state of a VirtualBox VM.")

    parser.add_argument('VM', type=checkvmtype, nargs="?", help="VirtualBox VM name")
    parser.add_argument('NEW_VM_NAME', type=str, nargs="?", help="VirtualBox VM name")

    parser.add_argument('--list-vms', action='store_true', help="list available vms")
    parser.add_argument('--list-hdds', action='store_true', help="list available vms")

    args = parser.parse_args()

    if args.list_vms:
        printVMs(vms)
        sys.exit(0)

    if args.list_hdds:
        for hdd in hddforest.getends():
            print("%s  (%s)" % (hdd.uuid, hdd.hdvm or ''))
            #print("%s" % hdd)
        sys.exit(0)

    if not args.VM:
        print("ERROR! Must specify VM.\n")
        parser.print_usage()
        print("\nVMs:")
        printVMs(vms)
        sys.exit(1)

    if not args.NEW_VM_NAME:
        print("ERROR! Must specify new VM name.\n")
        parser.print_usage()
        sys.exit(1)

    if args.NEW_VM_NAME in [vm.name for vm in vms]:
        print("ERROR! VM \"%s\" already exists.\n" % args.NEW_VM_NAME)
        parser.print_usage()
        sys.exit(1)

    tmp_vms = [vm for vm in vms if vm.uuid == args.VM or vm.name == args.VM]
    assert(len(tmp_vms) == 1)
    vm = tmp_vms[0]

    hdds = hddsattachedto(args.VM, hddforest)

    vm.fillininfo()

    # create new vm and fill in all applicable info from old vm
    newvm = createNewVM(args.NEW_VM_NAME, vm.info["ostype"], hddforest)
    newvm.setinfofrom(vm)

    print("Created new vm: %s" % newvm)


if __name__ == '__main__':
    main()
