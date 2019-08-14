"""This plugin provides Comment and Uncomment buttons in the toolbar
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS
import gps_utils

@gps_utils.hook('gps_started')
def __gps_started():
    GPS.Action('comment lines').button(
        toolbar='main', section='editor', label='Comment', icon='comment-symbolic')
    GPS.Action('uncomment lines').button(
        toolbar='main', section='editor', label='Uncomment', icon='uncomment-symbolic')
