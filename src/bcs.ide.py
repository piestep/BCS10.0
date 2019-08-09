"""This plugin provides ...
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS

def initialize_project_plugin():
    project_dirs = '''\
bcs
libs/argument
libs/boolean
libs/bcode
libs/parameter
libs/pcode
libs/pool
libs/system
'''

    test_dirs = '''\
bct
'''

    GPS.Project.root().set_property('project_dirs', project_dirs)
    GPS.Project.root().set_property('test_dirs', test_dirs)
    GPS.Project.set_scenario_variable("Build", "release")

def finalize_project_plugin():
    GPS.Project.root().remove_property('project_dirs')
    GPS.Project.root().remove_property('test_dirs')
    pass
