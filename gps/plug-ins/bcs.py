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
import string
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

  <submenu>
     <title>Help</title>
     <menu action="Boolean_compiler_system">
        <title>BCS</title>
     </menu>
     <submenu>
        <menu action="Test">
           <title>Test</title>
        </menu>
     </submenu>
  </submenu>

  <action name="Generate object directories">
     <description>Generate object directories for project</description>
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.generate_directories()</shell>
  </action>

  <action name="Generate unit test">
     <description>Generate unit test (not implemented)</description>
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.bcs_action()</shell>
  </action>

  <action name="Renumber errors">
     <description>Renumber error messages</description>
     <filter shell_lang="python" shell_cmd="bcs.in_project()" />
     <shell lang="python" output="none">bcs.renumber_errors()</shell>
  </action>

  <action name="Renumber messages">
     <description>Renumber test messages</description>
     <filter shell_lang="python" shell_cmd="bcs.in_test()" />
     <shell lang="python" output="none">bcs.renumber_messages()</shell>
  </action>

  <action name="Test scenario">
     <description>Replace test scenario in file</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
     </filter_and>
     <shell lang="python" output="none">bcs.scenario()</shell>
  </action>

  <action name="Test cases">
     <description>Replace test cases in file</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
     </filter_and>
     <shell lang="python" output="none">bcs.cases()</shell>
  </action>

  <action name="Replace scenarios">
     <description>Replace all test scenarios in project</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
     </filter_and>
     <shell lang="python" output="none">bcs.scenarios()</shell>
  </action>

  <action name="Replace registrations">
     <description>Replace all test case registrations in project</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
     </filter_and>
     <shell lang="python" output="none">bcs.registrations()</shell>
  </action>

  <submenu>
     <title>File</title>
     <menu><title/></menu>
     <submenu>
        <title>Project</title>
        <menu><title/></menu>
        <submenu>
           <title>BCS</title>
           <menu><title/></menu>
           <submenu>
              <title>Replace all</title>
              <menu action="Replace scenarios">
                 <title>Scenario</title>
              </menu>
              <menu action="Replace registrations">
                 <title>Cases</title>
              </menu>
           </submenu>
        </submenu>
     </submenu>
  </submenu>

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
        <submenu>
           <title>Test</title>
           <menu action="Test scenario">
              <title>Scenario</title>
           </menu>
           <menu action="Test cases">
              <title>Cases</title>
           </menu>
        </submenu>
    </submenu>
  </submenu>

  <action name="Open BCS project">
     <description>Open BCS project</description>
     <filter shell_lang="python" shell_cmd="bcs.is_test()" />
     <shell lang="python" output="none">bcs.to_project()</shell>
  </action>

  <action name="Open BCS test">
     <description>Open BCS test</description>
     <filter shell_lang="python" shell_cmd="bcs.is_project()" />
     <shell lang="python" output="none">bcs.to_test()</shell>
  </action>

  <action name="Test run">
     <description>Run standard test</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
        <filter shell_lang="python" shell_cmd="bcs.is_run()" />
     </filter_and>
     <shell lang="python" output="none">bcs.run()</shell>
     <external>%1</external>
  </action>

  <action name="Test dump">
     <description>Run standard test and dump results</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
        <filter shell_lang="python" shell_cmd="bcs.is_run()" />
     </filter_and>
     <shell lang="python" output="none">bcs.run()</shell>
     <external>%1 -d</external>
  </action>

  <action name="Test generate">
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
        <filter shell_lang="python" shell_cmd="bcs.is_run()" />
     </filter_and>
     <shell lang="python" output="none">bcs.run()</shell>
     <external>%1 -g</external>
  </action>

  <action name="Test replace">
     <description>Run standard test and replace results</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
        <filter shell_lang="python" shell_cmd="bcs.is_run()" />
     </filter_and>
     <shell lang="python" output="none">bcs.run()</shell>
     <external>%1 -g -r</external>
  </action>

  <action name="Test diff">
     <description>Run standard test and show diff of new and previous results</description>
     <filter_and>
        <filter language="ada" />
        <filter shell_lang="python" shell_cmd="bcs.is_test()" />
        <filter shell_lang="python" shell_cmd="bcs.in_test()" />
        <filter shell_lang="python" shell_cmd="bcs.is_run()" />
     </filter_and>
     <shell lang="python" output="none">bcs.run()</shell>
     <external>%1 -g</external>
     <shell lang="python" output="none">bcs.vdiff()</shell>
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
           <menu action="Test dump">
              <title>Dump</title>
           </menu>
           <menu action="Test generate">
              <title>Generate</title>
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

  <contextual action="Test scenario" >
    <Title>BCS/Code/Scenario</Title>
  </contextual>

  <contextual action="Test cases" >
    <Title>BCS/Code/Cases</Title>
  </contextual>

  <contextual action="Test run" >
    <Title>BCS/Analyze/Run</Title>
  </contextual>

  <contextual action="Test dump" >
    <Title>BCS/Analyze/Dump</Title>
  </contextual>

  <contextual action="Test generate" >
    <Title>BCS/Analyze/Generate</Title>
  </contextual>

  <contextual action="Test replace" >
    <Title>BCS/Analyze/Replace</Title>
  </contextual>

  <contextual action="Test diff" >
    <Title>BCS/Analyze/Diff</Title>
  </contextual>
 """)

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

def in_test():
   """
   in test
   """
   if is_project():
      return False

   elif is_test():
      name = GPS.Project.root().get_attribute_as_string("Test_Name", package="BCS")
      file = GPS.current_context().file()
      list = GPS.Project(name).sources(False)
      if file in list:
         return True

   return False

def in_project():
   """
   in project
   """
   if is_project():
      name = GPS.Project.root().get_attribute_as_string("Project_Name", package="BCS")
      file = GPS.current_context().file()
      list = GPS.Project(name).sources(True)
      if file in list:
         return True

   elif is_test():
      name = GPS.Project.root().get_attribute_as_string("Test_Name", package="BCS")
      file = GPS.current_context().file()
      list = GPS.Project(name).sources(False)
      if file not in list:
         return True

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
   GPS.Console("Messages").write("Renumbering errors: ")
   default = GPS.Project.root().get_attribute_as_string("Error_Pattern", package="PTest")
   if default == "":
      default = "(\".*)\(([SO]{0,1})[0-9]+\)\.\""
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
                  letter = match.group(2)
                  if key not in dict:
                     dict[key] = 1
                  else:
                     dict[key] = dict[key] + 1
                  buf.delete(loc[0], loc[1].forward_char(-1))
                  buf.insert(loc[0], key + " ("+letter+str(dict[key])+").\"")

                  count = count + 1
                  loc = loc[1].search(pattern[0], regexp=True, dialog_on_failure=False)

            if count == 0:
               GPS.Console("Messages").write("No error messages found\n")
            elif count == 1:
               GPS.Console("Messages").write("Renumbered 1 error message\n")
            else:
               GPS.Console("Messages").write("Renumbered " + str(count) + " error messages\n")
         else:
            GPS.Console("Messages").write("File (context) not selected.\n")

def renumber_messages():
   # renumber messages based on pattern in tests
   GPS.Console("Messages").write("Renumbering messages: ")
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

def is_run():
   """
   Is Run
   """
   dir, filename = os.path.split(GPS.Project.root().file().name())
   context = GPS.current_context()
   if context.entity_name() is not None:
      entity = context.entity()
      if entity is not None:
         if entity.category() == 'package':
            return entity.full_name().lower() == context.file().unit()
         elif entity.category() == 'procedure':
            return True

   return False

def run():
   """
   Run
   """
   dir = GPS.Project.root().exec_dir()
   main = GPS.Project.root().get_attribute_as_list("main")[0]
   file = GPS.File(main)
   prog = dir + GPS.Project.root().get_executable_name(file)

   context = GPS.current_context()
   if context.entity_name() is not None:
      entity = context.entity()
      if entity.category() == 'package':
         return prog + " -s " + entity.full_name().lower() + "!"
      elif entity.category() == 'procedure':
         senario = entity.full_name()[:-(len(entity.name())+1)].lower()
         case = entity.name().lower()
         return prog + " -s " + senario + "!" + " -c " + case + "!"

   return ""

def vdiff():
   """
   Run visual diff.
   """
   context = GPS.current_context()
   if context.entity_name() is not None:
      entity = context.entity()
      if entity.category() == 'procedure':
         pattern = GPS.Project.root().get_attribute_as_string("Test_Pattern", package="BCS")
         if pattern == "":
            pattern = ".+_Package\.(.+)_Test.*"
         match = re.match(pattern, entity.full_name())
         if match:
            package = match.group(1).lower()
            case = entity.name().lower()
            file1 = GPS.File("files/" + package + ".tmp")
            file2 = GPS.File("files/" + package + "/" + case + ".xml")
            GPS.Vdiff.create(file1, file2)


def bcs_action():
    print "ACTION"

def scenario():
   """
   Insert test scenario
   """

   context = GPS.current_context()
   file = context.file()
   name = file.unit()

   buf = GPS.EditorBuffer.get(context.file())
   cursor = buf.main_cursor()

   first = cursor.mark().location()
   last = cursor.sel_mark().location()
   buf.cut(first, last, False)

   GPS.Clipboard.copy("return AUnit.Format (\""+name+"!\");\n")
   cursor = buf.main_cursor()
   first = cursor.mark().location()
   buf.paste(first)


def cases():
   """
   Insert test cases
   """

   context = GPS.current_context()
   entities = context.file().entities(False)

   buf = GPS.EditorBuffer.get(context.file())
   cursor = buf.main_cursor()
   first = cursor.mark().location()
   last = cursor.sel_mark().location()
   buf.cut(first, last, False)

   for entity in entities:
      if entity.category() == 'procedure':
         if entity.name().startswith('Test_'):
            procedure = "      Register_Routine(The_Test, " + \
            entity.name() + \
            "\'Access, \"" + \
            entity.name().lower() + \
            "!\");\n"
            GPS.Clipboard.copy(procedure)
            cursor = buf.main_cursor()
            first = cursor.mark().location()
            buf.paste(first)


def scenarios():
   """
   Replace all scenarios in test project
   """

   count = 0
   pattern = "return AUnit.Format.*;"
   sources = GPS.Project.root().sources()
   for source in sources:
      if source.name()[-4:] == '.adb':
         file = open(source.name(), 'r')
         text = file.read()
         file.close
         match = re.search(pattern, text)
         if match is not None:
            text = re.sub(pattern, 'return AUnit.Format ("' + source.unit() + '!");', text)
            count = count + 1

            file = open(source.name(), 'w')
            file.write(text)
            file.close()

   if count == 0:
      print "No scenarios found."
   elif count == 1:
      print "1 scenerio found to replace."
   else:
      print str(count) + " scenarios found."


def registrations():
   """
   Replace all registrations in test project
   """

   count = 0
   sources = GPS.Project.root().sources()
   for source in sources:
      if source.name()[-4:] == '.adb':
         file = open(source.name(), 'r')
         text = file.read()
         file.close
         entities = source.entities(False)
         for entity in entities:
            if entity.category() == "procedure":
               pattern = "Register_Routine.*The_Test, " + entity.name() + "'Access" + '.*;'
               match = re.search(pattern, text)
               if match is not None:
                  replace = 'Register_Routine (The_Test, ' + entity.name() + \
                  "'Access, " + '"' + entity.name().lower() + \
                  '!");'
                  text = re.sub(pattern, replace, text)
                  count = count + 1

         file = open(source.name(), 'w')
         file.write(text)
         file.close()

   if count == 0:
      print "No registration test cases found."
   elif count == 1:
      print "1 registration test case found."
   else:
      print str(count) + " registration test cases found."


def bcs_test():
   """
   Test
   """
   print "TEST"


@gps_utils.hook('gps_started')
def __gps_started():
   if is_defined:
      GPS.Action('Open BCS project').button(toolbar='main', label='Project', icon='project-symbolic')
      GPS.Action('Open BCS test').button(toolbar='main', label='Test', icon='test-symbolic')

