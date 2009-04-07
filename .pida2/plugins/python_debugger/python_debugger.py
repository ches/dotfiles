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


# Standard library imports
import os, sys, time, Queue, cgi

# GTK imports
import gtk, gobject

# kiwi imports
from kiwi.ui.objectlist import ObjectList, ObjectTree, Column

# PIDA Imports
from pida.core.service import Service
from pida.core.features import FeaturesConfig
from pida.core.commands import CommandsConfig
from pida.core.events import EventsConfig
from pida.core.actions import ActionsConfig
from pida.core.options import OptionsConfig
from pida.core.actions import TYPE_NORMAL, TYPE_MENUTOOL, TYPE_RADIO, TYPE_TOGGLE
from pida.core.environment import get_uidef_path, get_pixmap_path

from pida.ui.views import PidaView
from pida.ui.terminal import PidaTerminal

from pida.utils import rpdb2

# locale
from pida.core.locale import Locale
locale = Locale('python_debugger')
_ = locale.gettext


# rpdb2 overrides to force PIDA terminal use


class SessionManagerInternal(rpdb2.CSessionManagerInternal):

    def _spawn_server(self, fchdir, ExpandedFilename, args, rid):
        """
        Start an OS console to act as server.
        What it does is to start rpdb again in a new console in server only mode.
        """
        debugger = os.path.abspath(rpdb2.__file__)
        if debugger[-1] == 'c':
            debugger = debugger[:-1]
        baseargs = ['python', debugger, '--debugee', '--rid=%s' % rid]
        if fchdir:
            baseargs.append('--chdir')
        if self.m_fAllowUnencrypted:
            baseargs.append('--plaintext')
        #if self.m_fRemote:
        #    baseargs.append('--remote')
        if os.name == 'nt':
            baseargs.append('--pwd=%s' % self.m_pwd)
        if 'PGD_DEBUG' in os.environ:
            baseargs.append('--debug')
        baseargs.append(ExpandedFilename)
        cmdargs = baseargs + args.split()
        python_exec = sys.executable
        self.terminal.fork_command(python_exec, cmdargs)


class SessionManager(rpdb2.CSessionManager):

    def __init__(self, manager, pwd, fAllowUnencrypted, fAllowRemote, host):
        self.manager = manager
        smi = self._CSessionManager__smi = SessionManagerInternal(
                            pwd, 
                            fAllowUnencrypted, 
                            fAllowRemote, 
                            host
                            )
        smi.terminal = self

    def _create_view(self):
        view = Terminal(self.app)
        self.main_window.attach_slave('outterm_holder', view)
        return view

    def fork_command(self, *args, **kw):
        self.manager.terminal_view.fork_command(*args, **kw)


class DebuggerManager(object):
    """Control the debugging process and views"""
    
    def __init__(self, svc):
        self.svc = svc
        rpdb2.main(self.start_client)
        self.last_step = None
        self.connect_events()
        sm = self.session_manager
        self.locals_view = LocalsViewer(sm)
        self.globals_view = GlobalsViewer(sm)
        self.stack_view = StackViewer(sm)
        self.threads_view = ThreadsViewer(sm)
        self.breaks_view = BreakpointViewer(sm)
        self.breaks_view.manager = self
        self.console_view = DebuggerConsole(sm)
        self.terminal_view = PidaTerminal()

    def start_client(self, command_line, fAttach, fchdir, pwd, fAllowUnencrypted, fRemote, host):
        self.session_manager = SessionManager(self, pwd, fAllowUnencrypted, fRemote, host)

    def launch(self, commandline, change_directory=False):
        gobject.idle_add(self.session_manager.launch, change_directory, commandline)

    def connect_events(self):
        event_type_dict = {rpdb2.CEventState: {}}
        self.session_manager.register_callback(self.on_update_state, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventStackFrameChange: {}}
        self.session_manager.register_callback(self.on_update_frame, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventThreads: {}}
        self.session_manager.register_callback(self.on_update_threads, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventNoThreads: {}}
        self.session_manager.register_callback(self.on_update_no_threads, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventNamespace: {}}
        self.session_manager.register_callback(self.on_update_namespace, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventThreadBroken: {}}
        self.session_manager.register_callback(self.on_update_thread_broken, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventStack: {}}
        self.session_manager.register_callback(self.on_update_stack, event_type_dict, fSingleUse = False)
        event_type_dict = {rpdb2.CEventBreakpoint: {}}
        self.session_manager.register_callback(self.on_update_bp, event_type_dict, fSingleUse = False)

    def on_update_state(self, event):
        def update():
            self.console_view.write_info(event.m_state + '\n')
            self.svc.update_state(event.m_state)
        gobject.idle_add(update)

    def on_update_frame(self, event):
        print 'uf', dir(event)
        gobject.idle_add(self.stack_view.select_frame, event.m_frame_index)
        self.on_update_source(event.m_frame_index)

    def on_update_namespace(self, *args):
        gobject.idle_add(self.locals_view.update_namespace)
        gobject.idle_add(self.globals_view.update_namespace)

    def on_update_stack(self, event):
        self.last_stack = event.m_stack
        gobject.idle_add(self.stack_view.update_stack, event.m_stack)
        self.on_update_source(-1)

    def on_update_source(self, frame_index):
        def update():
            self.remove_last_step_mark()
            stack_item = self.last_stack['stack'][frame_index]
            filename, linenumber, level, code = stack_item
            self.svc.boss.cmd('buffer', 'open_file', file_name=filename)
            self.svc.boss.editor.cmd('goto_line', line=linenumber)
            self.svc.boss.editor.cmd('show_sign', type='step', 
                                                    file_name=filename,
                                                    line=linenumber)
            self.last_step = filename, linenumber
        stack_item = self.last_stack['stack'][frame_index]
        if 'rpdb2.py' not in stack_item[0]:
            gobject.idle_add(update)

    def remove_last_step_mark(self):
        if self.last_step is not None:
            lfile, lline = self.last_step
            self.svc.boss.editor.cmd('hide_sign', type='step', file_name=lfile, line=lline) 
            self.last_step = None
            

    def on_update_bp(self, event):
        def _u(event):
            act = event.m_action
            if event.m_bp is not None:
                filename = event.m_bp.m_filename
                linenumber = event.m_bp.m_lineno
                index = event.m_bp.m_id
                indices = None
            else:
                filename = None
                linenumber = None
                index = None
                indices = event.m_id_list
            self.breaks_view.update_bp(act, index, indices, filename, linenumber)
            #self.master.update_bp(act, index, indices, filename, linenumber)
        gobject.idle_add(_u, event)

    def on_update_no_threads(self, *args):
        print 'unt', args

    def on_update_threads(self, event):
        gobject.idle_add(self.threads_view.update_threads, event.m_thread_list, event.m_current_thread)

    def on_update_thread_broken(self, *args):
        print 'utb', args

    def set_breakpoint(self, index, filename, linenumber):
        print 'set', index, filename, linenumber
        self.svc.boss.editor.cmd('show_sign', type='breakpoint', 
                                          file_name=filename,
                                          line=linenumber)

    def remove_breakpoint(self, index, filename, linenumber):
        print 'remove', index, filename, linenumber
        self.svc.boss.editor.cmd('hide_sign', type='breakpoint', 
                                          file_name=filename,
                                          line=linenumber)


# Views
class DebuggerConsole(gtk.VBox):

    def __init__(self, sm):
        gtk.VBox.__init__(self)
        self.sm = sm
        self.console = rpdb2.CConsole(sm, self, self, True)
        self.console.start()
        self._queue = Queue.Queue()
        self._text = gtk.TextView()
        sw = gtk.ScrolledWindow()
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        sw.add(self._text)
        self._buffer = self._text.get_buffer()
        self._buffer.create_tag('mono', family='Monospace')
        self._buffer.create_tag('bold', weight=800)
        self.pack_start(sw)
        self._entry = gtk.Entry()
        self.pack_start(self._entry, expand=False)
        self._entry.connect('activate', self.on_entry__activate)

    def write_text(self, data, *tags):
        self._buffer.insert_with_tags_by_name(
            self._buffer.get_end_iter(), data, 'mono', *tags)
        self._text.scroll_to_iter(self._buffer.get_end_iter(), 0.1)

    def write_info(self, data):
        gobject.idle_add(self.write_text, '[S] ' + data, 'bold')

    def write_from(self, data):
        gobject.idle_add(self.write_text, data)

    def write_to(self, data):
        if len(data) > 1:
            data = '>>> ' + data
            
        gobject.idle_add(self.write_text, data, 'bold')

    def write(self, data):
        self.write_from(data)

    def flush(self):
        pass

    def readline(self):
        return self._queue.get()

    def on_entry__activate(self, entry):
        data = self._entry.get_text() + '\n'
        self._entry.set_text('')
        self.write_to(data)
        self._queue.put(data)


nochildren = ['NoneType', 'str', 'int', 'float', 'long', 'bool']

reprable = nochildren + ['dict', 'list', 'tuple']


class NamespaceItem(object):

    def __init__(self, nsdict):
        self.name = nsdict['name']
        self.stype = nsdict['type']
        self.srepr = nsdict['repr']
        self.expr = nsdict['expr']
        self.n_subnodes = nsdict['n_subnodes']
        self.key = self.name
        self.is_value = False
        self.expanded = False
    
    def get_markup(self):
        if self.is_value:
            self.name = '.'
            mu = cgi.escape(self.srepr)
        else:
            n = cgi.escape(self.name)
            t = cgi.escape(self.stype)
            mu = ('<tt><b>%s</b>  </tt>'
                  '<span color="#903030"><i><small>%s</small></i></span>'
                  % (n, t))
            if self.stype in reprable:
                v = '<tt> %s</tt>' % cgi.escape(self.srepr)
                mu = ''.join([mu, v])
        return mu
    markup = property(get_markup)

    def get_pixbuf(self):
        if self.is_value:
            return None
        return get_pixbuf(self.stype)
    pixbuf = property(get_pixbuf)



class NamespaceViewer(gtk.VBox):

    def __init__(self, sm):
        gtk.VBox.__init__(self)
        self.session_manager = sm
        self._tree = ObjectTree(
            [
                Column('markup', use_markup=True)
            ]
        )
        self._tree.set_headers_visible(False)
        self.pack_start(self._tree)
        self._tree.connect('row-expanded', self.on_tree__row_expanded)
        
    def update_namespace(self, expr=None, parent=None):
        if expr is None:
            expr = self.get_root_expr()
            parent = None
            self._tree.clear()
        el = [(expr, True)]
        filt = None
        ns = self.session_manager.get_namespace(el, filt)
        for sn in ns[0]['subnodes']:
            item = NamespaceItem(sn)
            piter = self._tree.append(parent, item)
            if item.stype not in nochildren:
                valitem = NamespaceItem(sn)
                valitem.is_value = True
                self._tree.append(item, valitem)

    def on_tree__row_expanded(self, tv, item):
        if not item.expanded:
            item.expanded = True
            self.update_namespace(item.expr, item)

    def get_root_expr(self):
        raise NotImplementedError


class GlobalsViewer(NamespaceViewer):

    def get_root_expr(self):
        return 'globals()'


class LocalsViewer(NamespaceViewer):

    def get_root_expr(self):
        return 'locals()'




class StackItem(object):

    def __init__(self, index, filename, linenumber, function, line):
        self.key = index
        self.filename = filename
        self.basename = os.path.basename(filename)
        self.dirname = os.path.dirname(filename)
        self.linenumber = linenumber
        self.function = function
        self.line = line
        self.active=False


    def get_markup(self):
        return ('<span color="%(color)s">'
                '<b>%(basename)s:%(linenumber)s</b> '
                '<i><small>%(dirname)s</small></i>\n'
                '<tt>%(line)s</tt></span>' % dict(color=self.color,
                basename=self.basename, linenumber=self.linenumber,
                dirname=self.dirname, line=self.line))
    markup = property(get_markup)

    def get_color(self):
        if self.active:
            return '#000000'
        else:
            return '#909090'
    color = property(get_color)

    def get_icon(self):
        if self.active:
            return None#icons.get(gtk.STOCK_EXECUTE, 16)
        else:
            return None

    pixbuf = property(get_icon)


class StackViewer(gtk.VBox):

    def __init__(self, sm):
        self.session_manager = sm
        gtk.VBox.__init__(self)
        self.tree = ObjectList(
            [
                Column('markup', use_markup=True)
            ]
        )
        self.tree.set_headers_visible(False)
        self.pack_start(self.tree)
        self.tree.connect('double-click', self.on_tree__double_clicked)


    def update_stack(self, stack):
        self._current_tid = stack['current tid']
        self.tree.clear()
        stack_item = None
        for i, row in enumerate(stack['stack'][3:]):
            fn, ln, fc, tl = row
            stack_item = StackItem(i, fn, ln, fc, tl)
            self.tree.insert(0, stack_item)
        if stack_item is not None:
            stack_item.active = True
            self.tree.update(stack_item)
    
    def select_frame(self, index):
        for item in self.tree:
            item.active = (item.key == index)
            self.tree.update(item)

    def on_tree__double_clicked(self, tv, item):
        index = item.key
        self.session_manager.set_frame_index(index)


class ThreadItem(object):

    def __init__(self, tdict):
        self.tid = tdict[rpdb2.DICT_KEY_TID]
        self.broken = tdict[rpdb2.DICT_KEY_BROKEN]
        self.is_current = False
        self.key = self.tid

    def get_broken_text(self):
        if self.broken:
            return 'broken'
        else:
            return 'running'
    state = property(get_broken_text)

    def get_pixbuf(self):
        return None
    pixbuf = property(get_pixbuf)

    def get_markup(self):
        return  ('<tt>%(tid)s</tt> <span color="#909090"><i>%(state)s</i></span>'
           % dict(tid=self.tid, state=self.state) 
        )

    markup = property(get_markup)


class ThreadsViewer(gtk.VBox):

    def __init__(self, sm):
        gtk.VBox.__init__(self)
        self.sesion_manager = sm
        self.tree = ObjectList(
            [
                Column('markup', use_markup=True),
            ]
        )
        self.pack_start(self.tree)
        self.tree.set_headers_visible(False)

    def update_threads(self, threads_list, current_thread):
        self.tree.clear()
        for tdict in threads_list:
            item = ThreadItem(tdict)
            if item.tid == current_thread:
                item.is_current = True
            self.tree.append(item)

    def broken_thread(self, tid):
        for item in self.tree:
            if item.tid == tid:
                item.broken = True
                self.tree.update(item)

class Breakpoint(object):

    def __init__(self, index, filename, linenumber):
        self.key = index
        self.filename = filename
        self.linenumber = linenumber
        self.enabled = True

    def get_color(self):
        if self.enabled:
            return '#000000'
        else:
            return '#a0a0a0'

    def get_disabled_text(self):
        if self.enabled:
            return ''
        else:
            return '(disabled)'

    disabled_text = property(get_disabled_text)
    
    color = property(get_color)

    def get_markup(self):
        return ('<b>[%(key)s]</b>'
                '<span color="%(color)s">'
                '<tt> %(filename)s:%(linenumber)s </tt>'
                '</span><i>%(disabled_text)s</i>') % dict(
                    key=self.key, filename=self.filename,
                    disabled_text=self.disabled_text, color=self.color,
                    linenumber=self.linenumber,
                )

    markup = property(get_markup)


class BreakpointViewer(gtk.VBox):

    def __init__(self, sm):
        self.session_manager = sm
        gtk.VBox.__init__(self)
        self.tree = ObjectList(
            [
                Column('markup', use_markup=True),
            ]
        )
        self.tree.set_headers_visible(False)
        self.pack_start(self.tree)
        #self._create_actions()

    def update_bp(self, action, index, indices, filename, linenumber):
        if action == 'set':
            gen = self._get_all_bps([index])
            try:
                val = gen.next()
                val.filename = filename
                val.linenumber = linenumber
                self.tree.update(val)
            except StopIteration:
                bp = Breakpoint(index, filename, linenumber)
                self.tree.append(bp)
                self.manager.set_breakpoint(index, filename, linenumber)
        elif action == 'remove':
            for item in self._get_all_index_rows(indices):
                filename = item.filename
                self.manager.remove_breakpoint(item.key, item.filename, item.linenumber)
                self.tree.remove(item)
        elif action == 'disable':
            for item in self._get_all_bps(indices):
                item.enabled = False
                self.tree.update(item)
        elif action == 'enable':
            for item in self._get_all_bps(indices):
                item.enabled = True
                self.tree.update(item)


    def _create_actions(self):
        self._current = None
        self.add_widget('dis_act', gtk.Action('Disable', 'Disable',
            'Disable this breakpoint', gtk.STOCK_NO))
        self.add_widget('en_act', gtk.Action('Enable', 'Enable',
            'Enable this breakpoint', gtk.STOCK_YES))

    def _create_popup(self, bp, event):
        self._current = bp
        if not bp: return
        menu = gtk.Menu()
        mi = self.dis_act.create_menu_item()
        menu.add(mi)
        self.dis_act.set_sensitive(bp.enabled)
        mi = self.en_act.create_menu_item()
        menu.add(mi)
        self.en_act.set_sensitive(not bp.enabled)
        menu.show_all()
        menu.popup(None, None, None, event.button, event.time)

    def _get_all_index_rows(self, indices):
        for index in indices:
            for item in self.tree:
                if item.key == index:
                    yield item
        
    def _get_all_bps(self, indices):
        for item in self._get_all_index_rows(indices):
            yield item

    def _set_breakpoint_enabled_status(self, bp, enabled):
        if not enabled:
            func = self.session_manager.disable_breakpoint
        else:
            func = self.session_manager.enable_breakpoint
        gobject.idle_add(func, [bp.key], False)

    def on_tree__double_clicked(self, tv, item):
        if item:
            val = item.value
            self._set_breakpoint_enabled_status(val, not val.enabled)

    def on_tree__right_clicked(self, tv, item, event):
        if item is None:
            val = item
        else:
            val = item.value
        self._create_popup(val, event)
        
    def on_dis_act__activate(self, action):
        print 'disabled'
        self._set_breakpoint_enabled_status(self._current, False)

    def on_en_act__activate(self, action):
        self._set_breakpoint_enabled_status(self._current, True)

# PIDA View

class PythonDebuggerView(PidaView):

    def create_ui(self):
        self.manager = DebuggerManager(self.svc)
        self.create_toolbar()
        hp = gtk.HPaned()
        nb1 = gtk.Notebook()
        nb1.append_page(self.manager.console_view, gtk.Label('Console'))
        nb1.append_page(self.manager.terminal_view, gtk.Label('Execution Output'))
        nb1.append_page(self.manager.breaks_view, gtk.Label('Break Points'))
        hp.pack1(nb1)
        hb = gtk.HBox()
        nb2 = gtk.Notebook()
        nb2.append_page(self.manager.stack_view, gtk.Label('Stack'))
        nb2.append_page(self.manager.threads_view, gtk.Label('Threads'))
        nb2.append_page(self.manager.locals_view, gtk.Label('Locals'))
        nb2.append_page(self.manager.globals_view, gtk.Label('Globals'))
        hb.pack_start(nb2)
        hb.pack_start(self._toolbar, expand=False)
        hp.pack2(hb)
        self.add_main_widget(hp)
        hp.show_all()
        

    def create_toolbar(self):
        self._uim = gtk.UIManager()
        self._uim.insert_action_group(self.svc.get_action_group(), 0)
        self._uim.add_ui_from_file(get_uidef_path('python-debugger-toolbar.xml'))
        self._uim.ensure_update()
        self._toolbar = self._uim.get_toplevels('toolbar')[0]
        self._toolbar.set_style(gtk.TOOLBAR_ICONS)
        self._toolbar.set_icon_size(gtk.ICON_SIZE_SMALL_TOOLBAR)
        self._toolbar.set_orientation(gtk.ORIENTATION_VERTICAL)
        self._toolbar.show_all()



# Service Configuration

# Actions
class DebuggerActionsConfig(ActionsConfig):
    def create_actions(self):
        # Menu
        self.create_action(
            'show_pydebugger_view',
            TYPE_TOGGLE,
            'Python Debugger',
            'Show the Python debugger',
            'accessories-text-editor',
            self.on_show_debugger_view,
            '<Shift><Control>b',
        )
        #self.create_action(
        #    'show_stack_view',
        #    TYPE_TOGGLE,
        #    "Debugger's stack view",
        #    'Show the stack of current debugger',
        #    'accessories-text-editor',
        #    self.on_show_stack_view,
        #    '<Shift><Control>s',
        #)
    
        # Toolbar
        self.create_action(
            'debug_start',
            TYPE_NORMAL,
            'Continue',
            'Start debugger or Continue debbuging',
            'gdb-go',
            self.on_start,
            '<F3>',
        )
        #self.create_action(
        #    'debug_stop',
        #    TYPE_NORMAL,
        #    'Break',
        #    'Stop debbuging',
        #    'gdb-break',
        #    self.on_stop,
        #    '<F4>',
        #)
        self.create_action(
            'debug_next',
            TYPE_NORMAL,
            'Next',
            'Step to the next statement',
            'gdb-next',
            self.on_step_over,
            '<F6>',
        )
        self.create_action(
            'debug_step',
            TYPE_NORMAL,
            'Step',
            'Step into highlighted statement',
            'gdb-step',
            self.on_step_in,
            '<F5>',
        )
        self.create_action(
            'debug_return',
            TYPE_NORMAL,
            'Finish function',
            'Step until end of current function',
            'gdb-return',
            self.on_return,
            '<F7>',
        )
        self.create_action(
            'debug_break',
            TYPE_NORMAL,
            'Break',
            'Breakpoint the execution',
            'gdb-break',
            self.on_break,
            '<F3>',
        )

    # Buttonbar
    def on_step_over(self, action):
        self.svc._view.manager.session_manager.request_next()

    def on_step_in(self, action):
        self.svc._view.manager.session_manager.request_step()

    def on_start(self, action):
        self.svc._view.manager.session_manager.request_go()

    def on_stop(self, action):
        self.svc._view.manager.session_manager.stop_debuggee()

    def on_return(self, action):
        self.svc._view.manager.session_manager.request_return()

    # Menu
    def on_show_debugger_view(self, action):
        if action.get_active():
            self.svc.show_debugger_view()
        else:
            self.svc.boss.cmd('window', 'remove_view', view=self.svc._breakpoints_view)

    def on_show_stack_view(self, action):
        if not self.svc._stack_view:
            self.svc._stack_view = DebuggerStackView(self.svc)

        if action.get_active():
            self.svc.boss.cmd('window', 'add_view', paned='Terminal', view=self.svc._stack_view)
        else:
            self.svc.boss.cmd('window', 'remove_view', view=self.svc._stack_view)
        
    #
    def on_break(self, action):
        self.svc._view.manager.session_manager.request_break()


class DebuggerCommands(CommandsConfig):

    def launch(self, command_line, change_directory=False):
        self.svc.launch(command_line, change_directory)


class DebuggerEventsConfig(EventsConfig):

    def subscribe_foreign_events(self):
        #self.subscribe_foreign_event('buffer', 'document-changed',
        #                             self.on_document_changed)
        self.subscribe_foreign_event('editor', 'started',
                                     self.on_editor_startup)

    def on_editor_startup(self):
        """
        Set the highlights in vim
        """
        self.svc.boss.editor.cmd('define_sign_type', type="breakpoint", icon=get_pixmap_path("stop.svg"), 
                                                linehl="", text="X", texthl="Search")
        self.svc.boss.editor.cmd('define_sign_type', type="step", icon=get_pixmap_path("forward.svg"), 
                                                linehl="lCursor", text=">", texthl="lCursor")

    def on_document_changed(self, document):
        if document is not None:
            self.svc.get_action('debug_toggle_breakpoint').set_sensitive(True)
            self.svc.update_editor(document)
        else:
            self.svc.get_action('debug_toggle_breakpoint').set_sensitive(False)

# Service class
class Python_debugger(Service):
    """Describe your Service Here""" 
    actions_config = DebuggerActionsConfig
    commands_config = DebuggerCommands
    events_config = DebuggerEventsConfig

    def start(self):
        self._view = PythonDebuggerView(self)
        self.set_all_actions_insensitive()

    def show_debugger_view(self):
        self.boss.cmd('window', 'add_view', paned='Terminal', view=self._view)

    def hide_debugger_view(self):
        self.boss.cmd('window', 'remove_view', view=self._view)

    def ensure_view_visible(self):
        if not self.get_action('show_pydebugger_view').get_active():
            self.get_action('show_pydebugger_view').set_active(True)
        self.boss.cmd('window', 'present_view', view=self._view)

    def launch(self, command_line, change_directory):
        self.ensure_view_visible()
        self._view.manager.launch(command_line, change_directory)

    def update_state(self, state):
        if state == 'broken':
            self.set_broken_actions()
        elif state == 'running':
            self.set_running_actions()
        elif state == 'detached':
            self.set_all_actions_insensitive()
            self._view.manager.remove_last_step_mark()

        else:
            self.set_all_actions_insensitive()

    def set_all_actions_insensitive(self):
        for actname in ['debug_start', 'debug_next', 'debug_break',
                        'debug_step', 'debug_return']:
            self.get_action(actname).set_sensitive(False)

    def set_running_actions(self):
        self.set_all_actions_insensitive()
        for actname in ['debug_break']:
            self.get_action(actname).set_sensitive(True)

    def set_broken_actions(self):
        self.set_all_actions_insensitive()
        for actname in ['debug_start', 'debug_next', 'debug_step',
                        'debug_return']:
            self.get_action(actname).set_sensitive(True)

    def stop(self):
        try:
            self._view.manager.session_manager.stop_debuggee()
        except:
            pass


# Required Service attribute for service loading
Service = Python_debugger



# vim:set shiftwidth=4 tabstop=4 expandtab textwidth=79:
