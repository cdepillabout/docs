#!/usr/bin/env python

import os
import re
import sys

from subprocess import Popen, PIPE

class HDD:
    hduuid = None
    hdparent = None
    hdformat = None
    hdlocation = None
    hdstate = None
    hdtype = None
    hdusage = None

    parentHDD = None
    hddtree = None

    def __init__(self, lines, hddtree):
        assert(len(lines) == 7)
        self.hduuid = re.sub(r'^UUID:\W+', '', lines[0])
        self.hdparent = re.sub(r'^Parent UUID:\W+', '', lines[1])
        self.hdformat = re.sub(r'^Format:\W+', '', lines[2])
        self.hdlocation = re.sub(r'^Location:\W+', '', lines[3])
        self.hdstate = re.sub(r'^State:\W+', '', lines[4])
        self.hdtype = re.sub(r'^Type:\W+', '', lines[5])
        self.hdusage = re.sub(r'^Usage:\W+', '', lines[6])
        self.setTree(hddtree)

    def __str__(self):
        string = ""
        string += "uuid: %s\n" % self.hduuid
        string += "parent: %s\n" % self.hdparent
        string += "format: %s\n" % self.hdformat
        string += "location: %s\n" % self.hdlocation
        string += "state: %s\n" % self.hdstate
        string += "type: %s\n" % self.hdtype
        string += "usage: %s\n" % self.hdusage
        if self.parentHDD:
            string += "(parent: %s)\n" % self.parentHDD.hduuid
        if self.hddtree:
            string += "(children (%d total): %s)\n" % (len(self.hddtree), self.getChildren())
        return string

    def __repr__(self):
        return self.__str__()

    def setParent(self, parentHDD):
        print("Setting parent hdd %s in hdd %s\n" % (parentHDD.hduuid, self.hduuid))
        self.parentHDD = parentHDD

    def setTree(self, tree):
        self.hddtree = tree

    def getChildren(self):
        if self.hddtree:
            return [hdd.hduuid for hdd in self.hddtree if hdd.hdparent == self.hduuid]
        else:
            return []


class HDDTrees:

    hdds = []

    def __init__(self):
        pass

    def append(self, new_hdd):
        return self.addHDD(new_hdd)

    def addHDD(self, new_hdd):
        for hdd in self.hdds:
            if hdd.hduuid == new_hdd.hdparent:
                new_hdd.setParent(hdd)
            if hdd.hdparent == new_hdd.hduuid:
                hdd.setParent(new_hdd)
        self.hdds.append(new_hdd)

    def __iter__(self):
        return self.hdds.__iter__()

    def __len__(self):
        return len(self.hdds)

    def __str__(self):
        string = "%s hdds\n" % len(self.hdds)
        for hdd in self.hdds:
            string += "%s\n" % hdd

        return string

    def getEnds(self):
        return [hdd for hdd in self.hdds if not hdd.getChildren()]





def main():
    stdout = Popen(["VBoxManage", "list", "hdds"],
            stdout=PIPE).communicate()[0]
    stdout = stdout.decode('utf-8')
    stdout = re.sub('\n\n', '\n', stdout)
    stdout = re.sub('\n$', '', stdout)
    lines = stdout.split('\n')

    if len(lines) % 7 != 0:
        print("Expecting `VBoxManage list hdds | wc -l` to be a multiple of 7")
        print(len(lines) % 7)
        sys.exit(1)

    hdds = HDDTrees()
    for i in range(int(len(lines) / 7)):
        hdds.append(HDD(lines[i*7:(i+1)*7], hdds))
    print(hdds.getEnds())


if __name__ == '__main__':
    main()
