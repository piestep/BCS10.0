"""
This plug-in creates actions for the Boolean Compiler System.
"""

"""
Menus:
  /Code/BCS/Generate/Object directories
  	filter: is_defined
  /Code/BCS/Generate/Unit Test
        filter: is_project and source_code
  /Code/BCS/Renumber/Errors
        filter: is_project and source_code
  /Code/BCS/Renumber/Messages
        filter: is_test and source_code
  /Analyze/BCS/Open/Project
        filter: is_test
  /Analyze/BCS/Open/Test
        filter: is_project
  /Analyze/BCS/Test/Run
        filter: is_test and package or procedure selected
  /Analyze/BCS/Test/Listing
        filter: is_test and package or procedure selected
  /Analyze/BCS/Test/Dump
        filter: is_test and package or procedure selected
  /Analyze/BCS/Test/Replace
        filter: is_test and package or procedure selected
  /Analyze/BCS/Test/Diff
        filter: is_test and package or procedure selected

Context menus:
  /BCS/Run
        filter: is_test and package or procedure selected
  /BCS/Listing
        filter: is_test and package or procedure selected
  /BCS/Dump
        filter: is_test and package or procedure selected
  /BCS/Replace
        filter: is_test and package or procedure selected
  /BCS/Diff
        filter: is_test and package or procedure selected

"""

import os
import sys
import re
import GPS
import gps_utils
from xml.dom import minidom

GPS.parse_xml(
"""
  <project_attribute
     name="Project_Name"
     package="BCS"
     editor_page="BCS"
     editor_section="Project files"
     description="Project name"
     label="Project"
     omit_if_default="false" >

     <string default="" />
  </project_attribute>

  <project_attribute
     name="Project_File"
     package="BCS"
     editor_page="BCS"
     editor_section="Project files"
     description="Project file (.gpr)"
     label="Project"
     omit_if_default="false" >

     <string type="file" default="" />
  </project_attribute>

  <project_attribute
     name="Test_Name"
     package="BCS"
     editor_page="BCS"
     editor_section="Project files"
     description="Test project name"
     label="Test"
     omit_if_default="false">

     <string default="Test" />
  </project_attribute>

  <project_attribute
     name="Test_File"
     package="BCS"
     editor_page="BCS"
     editor_section="Project files"
     description="Test project file (.gpr)"
     label="Test"
     omit_if_default="false">

     <string type="file" default="" />
  </project_attribute>

  <project_attribute
     name="Message_Pattern"
     package="BCS"
     editor_page="BCS"
     editor_section="Patterns"
     description="Regular expression pattern for test messages"
     label="Messages"
     omit_if_default="false">

     <string default='\"\([0-9]+\)' />
  </project_attribute>

  <project_attribute
      name="Error_Pattern"
      package="BCS"
      editor_page="BCS"
      editor_section="Patterns"
      description="Regular expression pattern for error messages"
      label="Errors"
      omit_if_default="false">

      <string default='(\".*)(\([0-9]+\))(\.\")' />
  </project_attribute>

  <project_attribute
     name="Test_Pattern"
     package="BCS"
     editor_page="BCS"
     editor_section="Patterns"
     description="Regular expression pattern for test cases"
     label="Cases"
     omit_if_default="false">

     <string default='.+_Package\.(.+)_Test.*' />
  </project_attribute>

  <action name="Boolean_compiler_system">
     <shell lang="python" output="none">bcs.boolean_compiler_system()</shell>
  </action>

  <action name="Test">
     <shell lang="python" output="none">bcs.bcs_test()</shell>
  </action>

  <action name="In Test">
     <shell lang="python" output="none">bcs.in_test()</shell>
  </action>

  <action name="In Project">
     <shell lang="python" output="none">bcs.in_project()</shell>
  </action>

  <submenu>
     <title>Help</title>
     <menu action="Boolean_compiler_system">
        <title>BCS</title>
     </menu>
     <submenu>
        <menu action="Test">
           <title>Test1</title>
        </menu>
     </submenu>
     <submenu>
        <menu action="In Test">
           <title>In Test</title>
        </menu>
     </submenu>
     <submenu>
        <menu action="In Project">
           <title>In Project</title>
        </menu>
     </submenu>
  </submenu>

  <action name="Generate object directories">
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.generate_directories()</shell>
  </action>

  <action name="Generate unit test">
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <action name="Renumber errors">
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.renumber_errors()</shell>
  </action>

  <action name="Renumber messages">
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.renumber_messages()</shell>
  </action>

  <submenu>
     <title>Code</title>
     <menu><title/></menu>
     <submenu>
        <title>BCS</title>
        <submenu>
           <title>Generate</title>
           <menu action="Generate object directories">
              <title>Object directories</title>
           </menu>
           <menu action="Generate unit test">
              <title>Unit test</title>
           </menu>
        </submenu>
        <submenu>
           <title>Renumber</title>
           <menu action="Renumber errors">
              <title>Errors</title>
           </menu>
           <menu action="Renumber messages">
              <title>Messages</title>
           </menu>
        </submenu>
    </submenu>
  </submenu>

  <action name="Open BCS project">
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.to_project()</shell>
  </action>

  <action name="Open BCS test">
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.to_test()</shell>
  </action>

  <action name="Test run">
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <action name="Test list">
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <action name="Test dump">
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <action name="Test replace">
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <action name="Test diff">
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <submenu>
     <title>Analyze</title>
     <submenu>
        <title>BCS</title>
        <submenu>
           <title>Open</title>
           <menu action="Open BCS project"  after="GNATtest">
              <title>Project</title>
           </menu>
           <menu action="Open BCS test">
              <title>Test</title>
           </menu>
        </submenu>
        <submenu>
           <title>Test</title>
           <menu action="Test run">
              <title>Run</title>
           </menu>
           <menu action="Test list">
              <title>List</title>
           </menu>
           <menu action="Test dump">
              <title>Dump</title>
           </menu>
           <menu action="Test replace">
              <title>Replace</title>
           </menu>
           <menu action="Test diff">
              <title>Diff</title>
           </menu>
        </submenu>
    </submenu>
  </submenu>
 """)

def bcs_action():
    print "ACTION"

def is_defined():
   # return true if BCS project attribute is defined
   return GPS.Project.root().get_attribute_as_string("Project_File", package="BCS") is not ""

def is_project():
   # return false if BCS project attribute is not defined
   project = GPS.Project.root().get_attribute_as_string("Project_File", package="BCS")
   if project is not "":
      dir, filename = os.path.split(GPS.Project.root().file().name())
      return filename == project
   return False

def is_test():
   # return false if BCS test attribute is not defined
   test = GPS.Project.root().get_attribute_as_string("Test_File", package="BCS")
   if test is not "":
      dir, filename = os.path.split(GPS.Project.root().file().name())
      return filename == test
   return False

def to_project():
   # assumes BCS project attribute is defined and in test
   GPS.Project.load(GPS.Project.root().get_attribute_as_string("Project_File", package="BCS"))

def to_test():
   # assumes BCS test attribute is defined and in project
   GPS.Project.load(GPS.Project.root().get_attribute_as_string("Test_File", package="BCS"))

def bcs_mkdir(path):
   if not os.path.exists(path):
      os.makedirs(path)

def generate_directories():
   # create object directories from properties given in project 'ide.py' file
   obj_dir = GPS.Project.root().object_dirs()[0]
   project_dirs = GPS.Project.root().get_property('project_dirs')
   dirs = project_dirs.split();
   GPS.Console("Messages").write("Generating object directories\n")
   GPS.Console("Messages").write(os.path.abspath(os.path.join(obj_dir, 'test')) + '\n')
   bcs_mkdir(os.path.abspath(os.path.join(obj_dir, 'test')))
   for dir in dirs:
      GPS.Console("Messages").write(os.path.abspath(os.path.join(obj_dir, "..", dir)) + '\n')
      GPS.Console("Messages").write(os.path.abspath(os.path.join(obj_dir, "..", dir, 'test')) + '\n')
      bcs_mkdir(os.path.abspath(os.path.join(obj_dir, "..", dir)))
      bcs_mkdir(os.path.abspath(os.path.join(obj_dir, "..", dir, 'test')))

   test_dirs = GPS.Project.root().get_property('test_dirs')
   dirs = test_dirs.split();
   for dir in dirs:
      GPS.Console("Messages").write(os.path.abspath(os.path.join(obj_dir, "..", dir)) + '\n')
      bcs_mkdir(os.path.abspath(os.path.join(obj_dir, "..", dir)))


def renumber_errors():
   # renumber errors based on pattern in project
   GPS.Console("Messages").write("Renumbering errors")
   default = GPS.Project.root().get_attribute_as_string("Error_Pattern", package="PTest")
   if default == "":
      default = "(\".*)(\([0-9]+\))(\.\")"
   pattern = GPS.MDI.input_dialog("Please enter pattern", "Pattern="+default)
   if pattern:
      rexp = re.compile(pattern[0])
      dict = {}

      context = GPS.current_context()
      if context.file() is not None:
         filename = context.file().path
         if filename:
            buf = GPS.EditorBuffer.get(context.file())
            start = buf.beginning_of_buffer()
            count = 0
            with buf.new_undo_group():
               loc = start.search(pattern[0], regexp=True, dialog_on_failure=False)
               while loc:
                  s = buf.get_chars(loc[0], loc[1])
                  match = rexp.match(s)
                  key = match.group(1).strip()
                  if key not in dict:
                     dict[key] = 1
                  else:
                     dict[key] = dict[key] + 1
                  buf.delete(loc[0], loc[1].forward_char(-1))
                  buf.insert(loc[0], key + " ("+str(dict[key])+").\"")

                  count = count + 1
                  loc = loc[1].search(pattern[0], regexp=True, dialog_on_failure=False)

            if count == 0:
               GPS.Console("Messages").write("No error messages found\n")
            elif count == 1:
               GPS.Console("Messages").write("Renumbered 1 error message\n")
            else:
               GPS.Console("Messages").write("Renumbered "+str(count)+" error messages\n")
         else:
            GPS.Console("Messages").write("File (context) not selected.\n")

def renumber_messages():
   # renumber messages based on pattern in tests
   GPS.Console("Messages").write("Renumbering messages")
   default = GPS.Project.root().get_attribute_as_string("Message_Pattern", package="Pie")
   if default == "":
      default = "\"\([0-9]+\)"
   pattern = GPS.MDI.input_dialog("Please enter pattern", "Pattern="+default)
   if pattern:
      context = GPS.current_context()
      if context.file() is not None:
         filename = context.file().path
         if filename:
            buf = GPS.EditorBuffer.get(context.file())
            start = buf.beginning_of_buffer()
            count = 0
            with buf.new_undo_group():
               loc = start.search(pattern[0], regexp=True, dialog_on_failure=False)
               while loc:
                  buf.delete(loc[0], loc[1].forward_char(-1))
                  buf.insert(loc[0], "\"("+str(count+1)+")")
                  count = count + 1
                  loc = loc[1].search(pattern[0], regexp=True, dialog_on_failure=False)

            if count == 0:
               GPS.Console("Messages").write("No test messages found\n")
            elif count == 1:
               GPS.Console("Messages").write("Renumbered 1 test message\n")
            else:
               GPS.Console("Messages").write("Renumbered "+str(count)+" test messages\n")
      else:
         GPS.Console("Messages").write("File (context) not selected.\n")

def boolean_compiler_system():
    """
    Boolean Compiler System
    """
    GPS.MDI.dialog("BCS")

def bcs_test():
   """
   Test
   """
   print "TEST"
   # assume in source editor
   test = GPS.Project.root().get_attribute_as_string("Test_Name", package="BCS")
   project = GPS.Project.root().get_attribute_as_string("Project_Name", package="BCS")
   print test
   print project



def in_test():
   """
   in test
   """
   print "IN TEST"
   if is_project():
      print "in project - FALSE"

   elif is_test():
      name = GPS.Project.root().get_attribute_as_string("Test_Name", package="BCS")
      file = GPS.current_context().file()
      list = GPS.Project(name).sources(False)
      if file in list:
         print "in test - TRUE"
      else:
         print "not in test - FALSE"

def in_project():
   """
   in project
   """
   print "IN PROJECT"
   if is_project():
      name = GPS.Project.root().get_attribute_as_string("Project_Name", package="BCS")
      file = GPS.current_context().file()
      list = GPS.Project(name).sources(True)
      if file in list:
         print "in project - TRUE"
      else:
         print "not in project - FALSE"

   elif is_test():
      name = GPS.Project.root().get_attribute_as_string("Test_Name", package="BCS")
      file = GPS.current_context().file()
      list = GPS.Project(name).sources(False)
      if file in list:
         print "in test - FALSE"
      else:
         print "not in test - TRUE"

@gps_utils.hook('gps_started')
def __gps_started():
    pass

