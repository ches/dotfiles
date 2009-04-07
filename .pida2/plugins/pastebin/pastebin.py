# -*- coding: utf-8 -*- 

# Copyright (c) 2007 The PIDA Project

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.

import gtk, gobject

from kiwi.ui.objectlist import ObjectList, Column

# PIDA Imports
from pida.core.service import Service
from pida.core.features import FeaturesConfig
from pida.core.commands import CommandsConfig
from pida.core.events import EventsConfig
from pida.core.actions import ActionsConfig
from pida.core.actions import TYPE_NORMAL, TYPE_MENUTOOL, TYPE_RADIO, TYPE_TOGGLE

from pida.ui.views import PidaGladeView, PidaView

from pida.utils.web import fetch_url

# locale
from pida.core.locale import Locale
locale = Locale('pastebin')
_ = locale.gettext


class Bin(object):

    PASTE_URL = None

    def __init__(self, svc):
        self.svc = svc

    def create_data_dict(self, title, name, content, syntax):
        """Override in individual pastebins"""

    @classmethod
    def get_syntax_items(cls):
        """Override to return a list of syntax item tuples (lable, value)"""

    def post(self, *args):
        self.args = args
        fetch_url(self.PASTE_URL, self.on_posted, self.create_data_dict(*args))

    def on_posted(self, url, content):
        self.svc.new_paste_complete(url, *self.args)


class Dpaste(Bin):

    PASTE_URL = 'http://dpaste.com/'

    def create_data_dict(self, title, name, content, syntax):
        return dict(
            poster = name,
            title = title,
            content = content,
            language = syntax,
        )

    @classmethod
    def get_syntax_items(cls):
        return [
            ('Python', 'Python'),
            ('Python Interactive / Traceback', 'PythonConsole'),
            ('SQL', 'Sql'),
            ('HTML / Django Template', 'DjangoTemplate'),
            ('JavaScript', 'JScript'),
            ('CSS', 'Css'),
            ('XML', 'Xml'),
            ('Diff', 'Diff'),
            ('Ruby', 'Ruby'),
            ('Ruby HTML (ERB)', 'Rhtml'),
            ('Haskell', 'Haskell'),
            ('Apache Configuration', 'Apache'),
            ('Bash Script', 'Bash'),
            ('Plain Text', ''),
        ]


class Rafb(Bin):

    PASTE_URL = 'http://www.rafb.net/paste/paste.php'

    def create_data_dict(self, title, name, content, syntax):
        return dict(
            text=content,
            nick=name,
            desc=title,
            lang=syntax,
            cvt_tabs=4,
            submit=_('Paste')
        )

    @classmethod
    def get_syntax_items(cls):
        return [
            ('C89', 'C89'),
            ('C', 'C'),
            ('C++', 'C++'), 
            ('C#', 'C#'),
            ('Java', 'Java'),
            ('Pascal', 'Pascal'),
            ('Perl', 'Perl'),
            ('PHP', 'PHP'),
            ('PL/I', 'PL/I'),
            ('Python', 'Python'),
            ('Ruby', 'Ruby'),
            ('SQL', 'SQL'),
            ('VB', 'VB'),
            ('Plain Text', 'Plain Text')
        ]

class Twisted(Bin):

    PASTE_URL = 'http://deadbeefbabe.org/paste/freeform_post!!addPasting'

    def create_data_dict(self, title, name, content, syntax):
        return dict(
            author=name,
            text=content,
            addPasting='addPasting',
            _charset_='',
        )

    @classmethod
    def get_syntax_items(cls):
        return [('Python', '')]

class PastebinEditorView(PidaGladeView):

    gladefile = 'paste-editor'
    locale = locale
    label_text = _('Paste Editor')
    icon_name = gtk.STOCK_PASTE

    def create_ui(self):
        self.paste_location.prefill(self.svc.get_pastebin_types())

    def on_paste_location__content_changed(self, cmb):
        self.paste_syntax.prefill(self.paste_location.read().get_syntax_items())

    def on_post_button__clicked(self, button):
        paste_type = self.paste_location.read()
        self.svc.commence_paste(paste_type, *self.read_values())

    def on_cancel_button__clicked(self, button):
        self.svc.cancel_paste()

    def read_values(self):
        return (self.paste_title.get_text(),
                self.paste_name.get_text(),
                self.paste_content.get_buffer().get_text(
                    self.paste_content.get_buffer().get_start_iter(),
                    self.paste_content.get_buffer().get_end_iter(),
                ),
                self.paste_syntax.read(),
        )

    def can_be_closed(self):
        self.svc.cancel_paste()

class PasteHistoryView(PidaView):

    label_text = _('Paste History')
    icon_name = gtk.STOCK_PASTE

    #glade_file_name = 'paste-history.glade'

    def create_ui(self):
        self.__history_tree = ObjectList(
            [Column('markup', use_markup=True, expand=True)])
        self.__history_tree.set_headers_visible(False)
        self.add_main_widget(self.__history_tree)
        self.__x11_clipboard = gtk.Clipboard(selection="PRIMARY")
        self.__gnome_clipboard = gtk.Clipboard(selection="CLIPBOARD")
        self.__tree_selected = None
        #self.__history_tree.connect('selection-changed', self.cb_paste_clicked)
        self.__history_tree.connect('double-click', self.cb_paste_db_clicked)
        #self.__history_tree.connect('middle-clicked', self.cb_paste_m_clicked)
        self.__history_tree.connect('right-click', self.on_paste_rclick)
        self.__pulse_bar = gtk.ProgressBar()
        self.add_main_widget(self.__pulse_bar, expand=False)
        self.__pulse_bar.show_all()
        self.__pulse_bar.set_size_request(-1, 12)
        self.__pulse_bar.set_pulse_step(0.01)
        self.__history_tree.show_all()

    def set(self, pastes):
        '''Sets the paste list to the tree view.
           First reset it, then rebuild it.
        '''
        self.__history_tree.clear()
        for paste in pastes:
            self.__history_tree.append(paste)
        self.__tree_selected = None

    def add_paste(self, item):
        self.__history_tree.append(item)

    #def on_add__clicked(self, but):
    #    '''Callback function bound to the toolbar button new that creates a new
    #    paste to post'''
    #    self.service.boss.call_command('pastemanager','create_paste')

    def copy_current_paste(self):
        '''Callback function bound to the toolbar button view that copies the
        selected paste'''
        if self.__tree_selected != None:
            self.__x11_clipboard.set_text(self.__tree_selected.get_url())
            self.__gnome_clipboard.set_text(self.__tree_selected.get_url())

    def view_current_paste(self):
        '''Callback function bound to the toolbar button view that shows the
        selected paste'''
        if self.__tree_selected != None:
            self.service.boss.call_command('pastemanager','view_paste',
                paste=self.__tree_selected)
        else:
            print _("ERROR: No paste selected")

    def remove_current_paste(self):
        '''Callback function bound to the toolbar button delete that removes the
        selected paste'''
        if self.__tree_selected != None:
            self.service.boss.call_command('pastemanager','delete_paste',
                paste=self.__tree_selected)
        else:
            print _("ERROR: No paste selected")

    def cb_paste_clicked(self,paste,tree_item):
        '''Callback function called when an item is selected in the TreeView'''
        self.__tree_selected = tree_item.value

    def cb_paste_db_clicked(self, ol, item):
        """
        Callback function called when an item is double clicked, and copy it
        to the gnome/gtk clipboard
        """
        if item is not None:
            self.svc.boss.cmd('webbrowser', 'browse', url=item.url)
            # self.__gnome_clipboard.set_text(self.__tree_selected.get_url())
            # aa: view the paste

    def cb_paste_m_clicked(self,paste,tree_item):
        '''Callback function called when an item is middle clicked, and copy it
        to the mouse buffer clipboard'''
        if self.__tree_selected != None:
            self.__x11_clipboard.set_text(self.__tree_selected.get_url())

    def cb_paste_r_clicked(self, paste, tree_item, event):
        menu = gtk.Menu()
        sensitives = (tree_item is not None)
        for action in ['pastemanager+new_paste',
                        None,
                       'pastemanager+remove_paste',
                       'pastemanager+view_paste',
                        None,
                        'pastemanager+copy_url_to_clipboard']:
            if action is None:
                menu.append(gtk.SeparatorMenuItem())
            else:
                act = self.service.action_group.get_action(action)
                if 'new_paste' not in action:
                    act.set_sensitive(sensitives)
                mi = gtk.ImageMenuItem()
                act.connect_proxy(mi)
                mi.show()
                menu.append(mi)
        menu.show_all()
        menu.popup(None, None, None, event.button, event.time)

    def on_paste_rclick(self, ol, item, event):
        self.svc.boss.cmd('contexts', 'popup_menu', context='url-menu',
                          url=item.url, event=event)

    def start_pulse(self):
        '''Starts the pulse'''
        self._pulsing = True
        gobject.timeout_add(100, self._pulse)

    def stop_pulse(self):
        self._pulsing = False

    def _pulse(self):
        self.__pulse_bar.pulse()
        return self._pulsing

    def can_be_closed(self):
        self.svc.get_action('show_pastes').set_active(False)


class PastebinActionsConfig(ActionsConfig):

    def create_actions(self):
        self.create_action(
            'new_paste',
            TYPE_NORMAL,
            _('Upload Text Snippet'),
            _('Upload a text snippet to a pastebin'),
            gtk.STOCK_PASTE,
            self.on_new_paste,
        )

        self.create_action(
            'show_pastes',
            TYPE_TOGGLE,
            _('Paste History'),
            _('Show the paste history viewer'),
            gtk.STOCK_PASTE,
            self.on_show_pastes,
            '<Shift><Control>0',
        )

    def on_new_paste(self, action):
        self.svc.new_paste()

    def on_show_pastes(self, action):
        if action.get_active():
            self.svc.show_pastes()
        else:
            self.svc.hide_pastes()

class PasteItem(object):

    def __init__(self, url, *args):
        self.url = url
        self.title, self.name, self.content, self.syntax = args

    def get_markup(self):
        return ('<b>%s</b> (<span foreground="#0000c0">%s</span>)\n%s' %
                    (self.title, self.syntax, self.url))

    markup = property(get_markup)

# Service class
class Pastebin(Service):
    """Describe your Service Here""" 

    actions_config = PastebinActionsConfig

    def pre_start(self):
        self._editor = PastebinEditorView(self)
        self._view = PasteHistoryView(self)

    def new_paste(self):
        self.boss.cmd('window', 'add_view', paned='Plugin',
                      view=self._editor)
        self.get_action('new_paste').set_sensitive(False)

    def show_pastes(self):
        self.boss.cmd('window', 'add_view', paned='Plugin', view=self._view)

    def hide_pastes(self):
        self.boss.cmd('window', 'remove_view', view=self._view)

    def commence_paste(self, paste_type, *args):
        p = paste_type(self)
        p.post(*args)
        self.ensure_view_visible()
        self._view.start_pulse()
        self._close_paste_editor()

    def cancel_paste(self):
        self._close_paste_editor()
        
    def _close_paste_editor(self):
        self.boss.cmd('window', 'remove_view', view=self._editor)
        self.get_action('new_paste').set_sensitive(True)

    def new_paste_complete(self, url, *args):
        self._view.stop_pulse()
        self._view.add_paste(PasteItem(url, *args))
        self.ensure_view_visible()

    def ensure_view_visible(self):
        act = self.get_action('show_pastes')
        if not act.get_active():
            act.set_active(True)

    def get_pastebin_types(self):
        return [
            ('DPaste', Dpaste),
            ('Rafb.net', Rafb),
            #('Twisted', Twisted), #Broken for some reason
        ]

    def stop(self):
        if not self.get_action('new_paste').get_sensitive():
            self._close_paste_editor()
        if self.get_action('show_pastes').get_active():
            self.hide_pastes()

# Required Service attribute for service loading
Service = Pastebin



# vim:set shiftwidth=4 tabstop=4 expandtab textwidth=79:
