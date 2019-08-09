"""This plugin provides ...
"""

#############################################################################
# No user customization below this line
#############################################################################

def initialize_project_plugin():
    GPS.Project.set_scenario_variable("Build", "test")

def finalize_project_plugin():
    pass
