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

import gtk
import os
import cgi

from kiwi.ui.objectlist import ObjectList, Column

# PIDA Imports
from pida.core.service import Service
from pida.core.features import FeaturesConfig
from pida.core.commands import CommandsConfig
from pida.core.events import EventsConfig
from pida.core.actions import ActionsConfig
from pida.core.actions import TYPE_NORMAL, TYPE_MENUTOOL, TYPE_RADIO, TYPE_TOGGLE
from pida.core.environment import get_uidef_path

from pida.ui.views import PidaView

from pida.utils.gthreads import GeneratorTask, AsyncTask, gcall

# locale
from pida.core.locale import Locale
locale = Locale('bookmark')
_ = locale.gettext

class BookmarkItem(object):

    def __init__(self, group='none', title='no title', data=None):
        self.group = group
        self.title = title
        self.data = data

    def get_markup(self):
        return self.title
    markup = property(get_markup)

    def run(self, service):
        pass

    def key(self):
        return self.data + self.group + self.title


class BookmarkItemFile(BookmarkItem):

    def __init__(self, title='no title', data=None, line=1):
        self.line = line
        BookmarkItem.__init__(self, group='file', title=title, data=data)

    def key(self):
        return BookmarkItem.key(self) + str(self.line)

    def run(self, service):
        service.boss.cmd('buffer', 'open_file', file_name=self.data)
        service.boss.editor.goto_line(self.line)

class BookmarkItemPath(BookmarkItem):

    ICON_NAME = 'folder'

    def __init__(self, title='no title', data=None):
        BookmarkItem.__init__(self, group='path', title=title, data=data)

    def run(self, service):
        service.boss.cmd('filemanager', 'browse', new_path=self.data)
        service.boss.cmd('filemanager', 'present_view')

"""
class BookmarkItemUrl(BookmarkItem):

    ICON_NAME = 'www'

    def __init__(self, title='no title', data=None):
        BookmarkItem.__init__(self, group='url', title=title, data=data)

    def run(self, service):
        service.boss.call_command('webbrowser', 'browse', url=self.data)
"""


class BookmarkView(PidaView):

    icon_name = 'gtk-library'
    label_text = _('Bookmarks')

    def create_ui(self):
        self._vbox = gtk.VBox()
        self.create_toolbar()
        self.create_ui_list()
        self.add_main_widget(self._vbox)
        self._vbox.show_all()

    def create_tab_label(self, icon_name, text):
        if None in [icon_name, text]:
            return None
        label = gtk.Label(text)
        b_factory = gtk.HBox
        b = b_factory(spacing=2)
        icon = gtk.image_new_from_stock(icon_name, gtk.ICON_SIZE_MENU)
        b.pack_start(icon)
        b.pack_start(label)
        b.show_all()
        return b

    def create_ui_list(self):
        self._books = gtk.Notebook()
        self._books.set_border_width(6)
        self._list_dirs = ObjectList([Column('markup', data_type=str, use_markup=True)])
        self._list_dirs.connect('row-activated', self._on_item_activated)
        self._list_dirs.connect('selection-changed', self._on_item_selected)
        self._list_dirs.set_headers_visible(False)
        self._list_dirs.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self._books.append_page(self._list_dirs,
                tab_label=self.create_tab_label('stock_folder', _('Dirs')))
        self._list_files = ObjectList([Column('markup', data_type=str, use_markup=True)])
        self._list_files.connect('row-activated', self._on_item_activated)
        self._list_files.connect('selection-changed', self._on_item_selected)
        self._list_files.set_headers_visible(False)
        self._list_files.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self._books.append_page(self._list_files,
                tab_label=self.create_tab_label('text-x-generic', _('Files')))
        """
        self._list_url = ObjectList([Column('markup', data_type=str, use_markup=True)])
        self._list_url.set_headers_visible(False)
        self._books.add(self._list_url)
        """
        self._vbox.add(self._books)
        self._books.show_all()

    def create_toolbar(self):
        self._uim = gtk.UIManager()
        self._uim.insert_action_group(self.svc.get_action_group(), 0)
        self._uim.add_ui_from_file(get_uidef_path('bookmark-toolbar.xml'))
        self._uim.ensure_update()
        self._toolbar = self._uim.get_toplevels('toolbar')[0]
        self._toolbar.set_style(gtk.TOOLBAR_ICONS)
        self._toolbar.set_icon_size(gtk.ICON_SIZE_SMALL_TOOLBAR)
        self._vbox.pack_start(self._toolbar, expand=False)
        self._toolbar.show_all()

    def add_item(self, item):
        if item.group == 'file':
            self._list_files.append(item)
        elif item.group == 'path':
            self._list_dirs.append(item)
        elif item.group == 'url':
            self._list_urls.append(item)

    def remove_item(self, item):
        if item.group == 'file':
            self._list_files.remove(item)
        elif item.group == 'path':
            self._list_dirs.remove(item)
        elif item.group == 'url':
            self._list_urls.remove(item)

    def clear_all(self):
        self._list_files.clear()
        self._list_dirs.clear()
        #self._list_urls.clear()

    def can_be_closed(self):
        self.svc.get_action('show_bookmark').set_active(False)

    def _on_item_selected(self, olist, item):
        self.svc.set_current(item)

    def _on_item_activated(self, olist, item):
        item.run(self.svc)

class BookmarkActions(ActionsConfig):

    def create_actions(self):
        self.create_action(
            'show_bookmark',
            TYPE_TOGGLE,
            _('Bookmark Viewer'),
            _('Show bookmarks'),
            '',
            self.on_show_bookmark,
            '<Shift><Control>1',
        )

        self.create_action(
            'bookmark_curfile',
            TYPE_NORMAL,
            _('Bookmark current file'),
            _('Bookmark current file'),
            'text-x-generic',
            self.on_bookmark_curfile,
            'NOACCEL',
        )

        self.create_action(
            'bookmark_curdir',
            TYPE_NORMAL,
            _('Bookmark current directory'),
            _('Bookmark current directory'),
            'stock_folder',
            self.on_bookmark_curdir,
            'NOACCEL',
        )

        self.create_action(
            'bookmark_delsel',
            TYPE_NORMAL,
            _('Delete selected item'),
            _('Delete selected item'),
            gtk.STOCK_DELETE,
            self.on_bookmark_delsel,
            'NOACCEL',
        )

        self.create_action(
            'bookmark-for-dir',
            TYPE_NORMAL,
            _('Add as bookmark'),
            _('Add selected directory as bookmark'),
            gtk.STOCK_ADD,
            self.on_bookmark_for_dir,
            'NOACCEL',
        )

        self.create_action(
            'bookmark-for-file',
            TYPE_NORMAL,
            _('Add as bookmark'),
            _('Add selected file as bookmark'),
            gtk.STOCK_ADD,
            self.on_bookmark_for_file,
            'NOACCEL',
        )


    def on_show_bookmark(self, action):
        if action.get_active():
            self.svc.show_bookmark()
        else:
            self.svc.hide_bookmark()

    def on_bookmark_curdir(self, action):
        self.svc.bookmark_dir()

    def on_bookmark_curfile(self, action):
        self.svc.bookmark_file()

    def on_bookmark_delsel(self, action):
        self.svc.remove_current()

    def on_bookmark_for_file(self, action):
        self.svc.bookmark_file(filename=action.contexts_kw['file_name'])

    def on_bookmark_for_dir(self, action):
        self.svc.bookmark_dir(path=action.contexts_kw['dir_name'])

class BookmarkFeatures(FeaturesConfig):

    def subscribe_foreign_features(self):
        self.subscribe_foreign_feature('contexts', 'file-menu',
            (self.svc.get_action_group(), 'bookmark-file-menu.xml'))
        self.subscribe_foreign_feature('contexts', 'dir-menu',
            (self.svc.get_action_group(), 'bookmark-dir-menu.xml'))

class BookmarkEvents(EventsConfig):

    def subscribe_foreign_events(self):
        self.subscribe_foreign_event('project', 'project_switched',
                                     self.svc.on_project_switched)



# Service class
class Bookmark(Service):
    """Manage bookmarks"""

    actions_config = BookmarkActions
    features_config = BookmarkFeatures
    events_config = BookmarkEvents

    def start(self):
        self._view = BookmarkView(self)
        self._has_loaded = False
        self._items = []
        self._current = None
        self._project = None

    def show_bookmark(self):
        self.boss.cmd('window', 'add_view', paned='Plugin', view=self._view)
        if not self._has_loaded:
            self._has_loaded = True

    def hide_bookmark(self):
        self.boss.cmd('window', 'remove_view', view=self._view)


    def set_current(self, item):
        self._current = item

    def add_item(self, item):
        for t in self._items:
            if t.key() == item.key():
                return
        self._items.append(item)
        self._view.add_item(item)
        self.save()

    def remove_current(self):
        if self._current == None:
            return
        self._items.remove(self._current)
        self._view.remove_item(self._current)
        self._current = None
        self.save()

    def bookmark_dir(self, path=None):
        if path == None:
            path = self.boss.cmd('filemanager', 'get_browsed_path')
        source_directory = self.boss.cmd('project', 'get_current_project').source_directory
        title = path
        if title.startswith(source_directory):
            title = title[len(source_directory):]
            if title == '':
                title = _('(project root)')
            else:
                title = '.' + title
        item = BookmarkItemPath(title=title, data=path)
        self.add_item(item)

    def bookmark_file(self, filename=None, line=None):
        if filename == None:
            document = self.boss.cmd('buffer', 'get_current')
            if document == None:
                return
            filename = document.get_filename()
        if line == None:
            line = self.boss.editor.cmd('get_current_line_number')
        filename_title = os.path.basename(filename)
        title = '%s:<span color="#000099">%d</span>' % (
                cgi.escape(filename_title), int(line))
        item = BookmarkItemFile(title=title, data=filename, line=line)
        self.add_item(item)

    def on_project_switched(self, project):
        if project != self._project:
            self._project = project
            self.load()

    def _serialize(self):
        data = {}
        for t in self._items:
            if not data.has_key(t.group):
                data[t.group] = []
            if t.group == 'file':
                data[t.group].append('%s:%d' % (t.data, int(t.line)))
            else:
                data[t.group].append(t.data)
        return data

    def _unserialize(self, data):
        if data == None:
            return
        for key in data:
            items = data[key]
            for item in items:
                if key == 'file':
                    t = item.rsplit(':')
                    self.bookmark_file(filename=t[0], line=t[1])
                elif key == 'path':
                    self.bookmark_dir(path=item)

    def load(self):
        self._items = []
        self._view.clear_all()
        data = self.boss.cmd('project', 'get_current_project_data',
                section_name='bookmark')
        self._unserialize(data)

    def save(self):
        data = self._serialize()
        self.boss.cmd('project', 'save_to_current_project',
                section_name='bookmark', section_data=data)

    def stop(self):
        if self.get_action('show_bookmark').get_active():
            self.hide_bookmark()


# Required Service attribute for service loading
Service = Bookmark



# vim:set shiftwidth=4 tabstop=4 expandtab textwidth=79:
