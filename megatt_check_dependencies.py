# This script is used to check if dependencies have changed for the megatt
# executable on Linux systems since we want to ensure that we don't have 
# dependencies on graphical libraries (e.g. GTK) since megatt runs
# on a headless server. If dependencies have changed, we must then manually
# check if they prevent megatt from running on a headless server. If so, we 
# refactor the megatt source code to remove the dependency, otherwise we must 
# update this script to accept the new dependency.

__author__ = " Glen Stecher <gstecher@bd3software.com>"
__date__ = "$Oct 25, 2018 8:26:28 PM$"

from MegattDependencyTest import MegattDependencyTest

if __name__ == "__main__":
    print "Running megatt dependency test"
    test = MegattDependencyTest()
    if test.run_test() != True:
        print "DEPENDENCY TEST FAILED! Check that no dependencies on graphical libraries exist"
    else:
        print "Dependency test passed!"
