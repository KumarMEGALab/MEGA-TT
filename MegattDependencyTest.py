# To change this license header, choose License Headers in Project Properties.
# To change this template file, choose Tools | Templates
# and open the template in the editor.

__author__ = "Glen Stecher"
__date__ = "$Oct 25, 2018 8:39:10 PM$"

import os.path
import sys
from subprocess import Popen, PIPE


class MegattDependencyTest:

    def __init__(self):
        self.expected_dependencies = []
        self.megatt = './megatt'
        self.init_expected_dependencies()
        self.current_dependencies = []

    def run_test(self):
        result = False
        debug = self.current_ldd_result()
        return self.compare()

    def expected_ldd_result(self):
        return ''

    def current_ldd_result(self):
        process = Popen(["ldd", self.megatt], stdout=PIPE, stderr=PIPE, encoding='utf8')
        stdout, stderr = process.communicate()        
        lines = stdout.split("\n")
        for line in lines:
            if line.strip() != '':
                self.add_to_current(line)
        return stdout

    def add_to_current(self, line):
        tokens = line.split("=")
        temp = tokens[0]
        if temp.find("(") >= 0:
            temp = temp[0:temp.find("(")]
        self.current_dependencies.append(temp.strip())
        print("     megatt depends on: " + temp)

    def compare(self):
        result = True
        if len(self.expected_dependencies) != len(self.current_dependencies):
            result = False
        for i, d in enumerate(self.expected_dependencies):
            if self.current_dependencies[i] != d:
                result = False

        if not result:
            print("     TEST FAILED - dumping data:")
            print("         current dependencies:")
            for d in self.current_dependencies:
                print("             " + d)
            print("         expected dependencies:")
            for d in self.expected_dependencies:
                print("             " + d)
        return result

    def init_expected_dependencies(self):
        self.expected_dependencies.append('linux-vdso.so.1')
        #self.expected_dependencies.append('libpthread.so.0')
        #self.expected_dependencies.append('libdl.so.2')
        self.expected_dependencies.append('libc.so.6')
        self.expected_dependencies.append('/lib64/ld-linux-x86-64.so.2')
