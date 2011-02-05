import os
import sys

import paste.fixture
import paste.registry
from paste.deploy import loadapp
from paste.script.command import Command, BadCommand
from paste.script.filemaker import FileOp
from tempita import paste_script_template_renderer

import pylons
import pylons.util as util

__all__ = ['ControllerCommand', 'RestControllerCommand', 'ShellCommand']

def can_import(name):
    
    try:
        __import__(name)
        return True
    except ImportError:
        return False

def is_minimal_template(package, fail_fast=False):
    
    minimal_template = False
    try:
        __import__(package + '.lib.base')
    except ImportError, ie:
        if 'No module named lib.base' in str(ie):
            minimal_template = True
    except:
        if fail_fast:
            raise
    return minimal_template

def defines_render(package):
    
    base_module = (is_minimal_template(package) and package + '.controllers' or
                   package + '.lib.base')
    try:
        base = __import__(base_module, globals(), locals(), ['__doc__'])
    except:
        return False
    return callable(getattr(base, 'render', None))

def validate_name(name):
    
    if not name:
        raise BadCommand('Please give the name of a controller.')
    if name != 'setup' and can_import(name):
        raise BadCommand(
            "\n\nA module named '%s' is already present in your "
            "PYTHON_PATH.\nChoosing a conflicting name will likely cause "
            "import problems in\nyour controller at some point. It's "
            "suggested that you choose an\nalternate name, and if you'd "
            "like that name to be accessible as\n'%s', add a route "
            "to your projects config/routing.py file similar\nto:\n"
            "    map.connect('%s', controller='my_%s')" \
            % (name, name, name, name))
    return True

def check_controller_existence(base_package, directory, name): 
    
    filename = os.path.join(base_package, 'controllers', directory,
                            name + '.py')
    if os.path.exists(filename): 
        raise BadCommand('Controller %s already exists.' %
                         os.path.join(directory, name))

class ControllerCommand(Command):
    
    summary = __doc__.splitlines()[0]
    usage = '\n' + __doc__

    min_args = 1
    max_args = 1
    group_name = 'pylons'

    default_verbosity = 3

    parser = Command.standard_parser(simulate=True)
    parser.add_option('--no-test',
                      action='store_true',
                      dest='no_test',
                      help="Don't create the test; just the controller")

    def command(self):
        
        try:
            file_op = FileOp(source_dir=('pylons', 'templates'))
            try:
                name, directory = file_op.parse_path_name_args(self.args[0])
            except:
                raise BadCommand('No egg_info directory was found')

            base_package = file_op.find_dir('controllers', True)[0]
            if base_package.lower() == name.lower():
                raise BadCommand(
                    'Your controller name should not be the same as '
                    'the package name %r.' % base_package)
            name = name.replace('-', '_')
            validate_name(name)

            if is_minimal_template(base_package):
                importstatement = ('from %s.controllers import BaseController'
                                   % base_package)
            else:
                importstatement = ('from %s.lib.base import BaseController' %
                                   base_package)
            if defines_render(base_package):
                importstatement += ', render'

            fullname = os.path.join(directory, name)
            controller_name = util.class_name_from_module_name(
                name.split('/')[-1])
            if not fullname.startswith(os.sep):
                fullname = os.sep + fullname
            testname = fullname.replace(os.sep, '_')[1:]
            
            module_dir = directory.replace('/', os.path.sep)
            check_controller_existence(base_package, module_dir, name)
            
            file_op.template_vars.update(
                {'name': controller_name,
                 'fname': os.path.join(directory, name).replace('\\', '/'),
                 'tmpl_name': name,
                 'package':base_package,
                 'importstatement': importstatement})
            file_op.copy_file(template='controller.py_tmpl',
                              dest=os.path.join('controllers', directory),
                              filename=name,
                              template_renderer=paste_script_template_renderer)
            if not self.options.no_test:
                file_op.copy_file(
                    template='test_controller.py_tmpl',
                    dest=os.path.join('tests', 'functional'),
                    filename='test_' + testname,
                    template_renderer=paste_script_template_renderer)
        except BadCommand, e:
            raise BadCommand('An error occurred. %s' % e)
        except:
            msg = str(sys.exc_info()[1])
            raise BadCommand('An unknown error occurred. %s' % msg)

class RestControllerCommand(Command):
    
    summary = __doc__.splitlines()[0]
    usage = '\n' + __doc__

    min_args = 2
    max_args = 2
    group_name = 'pylons'

    default_verbosity = 3

    parser = Command.standard_parser(simulate=True)
    parser.add_option('--no-test',
                      action='store_true',
                      dest='no_test',
                      help="Don't create the test; just the controller")

    def command(self):
        
        try:
            file_op = FileOp(source_dir=('pylons', 'templates'))
            try:
                singularname, singulardirectory = \
                    file_op.parse_path_name_args(self.args[0])
                pluralname, pluraldirectory = \
                    file_op.parse_path_name_args(self.args[1])

            except:
                raise BadCommand('No egg_info directory was found')

            base_package = file_op.find_dir('controllers', True)[0]
            if base_package.lower() == pluralname.lower():
                raise BadCommand(
                    'Your controller name should not be the same as '
                    'the package name %r.'% base_package)
            for name in [pluralname]:
                name = name.replace('-', '_')
                validate_name(name)

            if is_minimal_template(base_package):
                importstatement = ('from %s.controllers import BaseController'
                                   % base_package)
            else:
                importstatement = ('from %s.lib.base import BaseController' %
                                   base_package)
            if defines_render(base_package):
                importstatement += ', render'
            
            module_dir = pluraldirectory.replace('/', os.path.sep)
            check_controller_existence(base_package, module_dir, name)
            
            fullname = os.path.join(pluraldirectory, pluralname)
            controller_name = util.class_name_from_module_name(
                pluralname.split('/')[-1])
            if not fullname.startswith(os.sep):
                fullname = os.sep + fullname
            testname = fullname.replace(os.sep, '_')[1:]

            nameprefix = ''
            path = ''
            if pluraldirectory:
                nameprefix = pluraldirectory.replace(os.path.sep, '_') + '_'
                path = pluraldirectory + '/'
                
            controller_c = ''
            if nameprefix:
                controller_c = ", controller='%s', \n\t" % \
                    '/'.join([pluraldirectory, pluralname])
                controller_c += "path_prefix='/%s', name_prefix='%s'" % \
                    (pluraldirectory, nameprefix)
            command = "map.resource('%s', '%s'%s)\n" % \
                (singularname, pluralname, controller_c)

            file_op.template_vars.update(
                {'classname': controller_name,
                 'pluralname': pluralname,
                 'singularname': singularname,
                 'name': controller_name,
                 'nameprefix': nameprefix,
                 'package':base_package,
                 'path':path,
                 'resource_command': command.replace('\n\t', '\n%s#%s' % \
                                                         (' '*4, ' '*9)),
                 'fname': os.path.join(pluraldirectory, pluralname),
                 'importstatement': importstatement})

            resource_command = ("\nTo create the appropriate RESTful mapping, "
                                "add a map statement to your\n")
            resource_command += ("config/routing.py file near the top like "
                                 "this:\n\n")
            resource_command += command
            file_op.copy_file(template='restcontroller.py_tmpl',
                              dest=os.path.join('controllers', pluraldirectory),
                              filename=pluralname,
                              template_renderer=paste_script_template_renderer)
            if not self.options.no_test:
                file_op.copy_file(
                    template='test_restcontroller.py_tmpl',
                    dest=os.path.join('tests', 'functional'),
                    filename='test_' + testname,
                    template_renderer=paste_script_template_renderer)
            print resource_command
        except BadCommand, e:
            raise BadCommand('An error occurred. %s' % e)
        except:
            msg = str(sys.exc_info()[1])
            raise BadCommand('An unknown error occurred. %s' % msg)

class RoutesCommand(Command):
    
    summary = __doc__.splitlines()[0]
    usage = '\n' + __doc__

    min_args = 0
    max_args = 1
    group_name = 'pylons'

    parser = Command.standard_parser(simulate=True)
    parser.add_option('-q',
                      action='count',
                      dest='quiet',
                      default=0,
                      help=("Do not load logging configuration from the "
                            "config file"))

    def command(self):
        
        self.verbose = 3
        if len(self.args) == 0:
            config_file = 'development.ini'
            if not os.path.isfile(config_file):
                raise BadCommand('%sError: CONFIG_FILE not found at: .%s%s\n'
                                 'Please specify a CONFIG_FILE' % \
                                 (self.parser.get_usage(), os.path.sep,
                                  config_file))
        else:
            config_file = self.args[0]

        config_name = 'config:%s' % config_file
        here_dir = os.getcwd()

        if not self.options.quiet:
            self.logging_file_config(config_file)
        
        wsgiapp = loadapp(config_name, relative_to=here_dir)
        test_app = paste.fixture.TestApp(wsgiapp)

        tresponse = test_app.get('/_test_vars')
        mapper = tresponse.config.get('routes.map')
        if mapper:
            print mapper

class ShellCommand(Command):
    
    summary = __doc__.splitlines()[0]
    usage = '\n' + __doc__

    min_args = 0
    max_args = 1
    group_name = 'pylons'

    parser = Command.standard_parser(simulate=True)
    parser.add_option('-d', '--disable-ipython',
                      action='store_true',
                      dest='disable_ipython',
                      help="Don't use IPython if it is available")

    parser.add_option('-q',
                      action='count',
                      dest='quiet',
                      default=0,
                      help=("Do not load logging configuration from the "
                            "config file"))

    def command(self):
        
        self.verbose = 3
        if len(self.args) == 0:
            config_file = 'development.ini'
            if not os.path.isfile(config_file):
                raise BadCommand('%sError: CONFIG_FILE not found at: .%s%s\n'
                                 'Please specify a CONFIG_FILE' % \
                                 (self.parser.get_usage(), os.path.sep,
                                  config_file))
        else:
            config_file = self.args[0]

        config_name = 'config:%s' % config_file
        here_dir = os.getcwd()
        locs = dict(__name__="pylons-admin")

        if not self.options.quiet:
            self.logging_file_config(config_file)
        
        sys.path.insert(0, here_dir)

        wsgiapp = loadapp(config_name, relative_to=here_dir)
        test_app = paste.fixture.TestApp(wsgiapp)

        tresponse = test_app.get('/_test_vars')
        request_id = int(tresponse.body)

        test_app.pre_request_hook = lambda self: \
            paste.registry.restorer.restoration_end()
        test_app.post_request_hook = lambda self: \
            paste.registry.restorer.restoration_begin(request_id)

        paste.registry.restorer.restoration_begin(request_id)
                
        pkg_name = pylons.config['pylons.package']

        if is_minimal_template(pkg_name, True):
            model_module = None
            helpers_module = pkg_name + '.helpers'
            base_module = pkg_name + '.controllers'
        else:
            model_module = pkg_name + '.model'
            helpers_module = pkg_name + '.lib.helpers'
            base_module = pkg_name + '.lib.base'

        if model_module and can_import(model_module):
            locs['model'] = sys.modules[model_module]

        if can_import(helpers_module):
            locs['h'] = sys.modules[helpers_module]

        exec ('from pylons import app_globals, config, request, response, '
              'session, tmpl_context, url') in locs
        exec ('from pylons.controllers.util import abort, redirect') in locs
        exec 'from pylons.i18n import _, ungettext, N_' in locs
        locs.pop('__builtins__', None)
        
        __import__(base_module)

        base = sys.modules[base_module]
        base_public = [__name for __name in dir(base) if not \
                       __name.startswith('_') or __name == '_']
        locs.update((name, getattr(base, name)) for name in base_public)
        locs.update(dict(wsgiapp=wsgiapp, app=test_app))

        mapper = tresponse.config.get('routes.map')
        if mapper:
            locs['mapper'] = mapper

        banner = "  All objects from %s are available\n" % base_module
        banner += "  Additional Objects:\n"
        if mapper:
            banner += "  %-10s -  %s\n" % ('mapper', 'Routes mapper object')
        banner += "  %-10s -  %s\n" % ('wsgiapp',
            "This project's WSGI App instance")
        banner += "  %-10s -  %s\n" % ('app',
            'paste.fixture wrapped around wsgiapp')

        try:
            if self.options.disable_ipython:
                raise ImportError()

            from IPython.Shell import IPShellEmbed

            shell = IPShellEmbed(argv=self.args)
            shell.set_banner(shell.IP.BANNER + '\n\n' + banner)
            try:
                shell(local_ns=locs, global_ns={})
            finally:
                paste.registry.restorer.restoration_end()
        except ImportError:
            import code
            py_prefix = sys.platform.startswith('java') and 'J' or 'P'
            newbanner = "Pylons Interactive Shell\n%sython %s\n\n" % \
                (py_prefix, sys.version)
            banner = newbanner + banner
            shell = code.InteractiveConsole(locals=locs)
            try:
                import readline
            except ImportError:
                pass
            try:
                shell.interact(banner)
            finally:
                paste.registry.restorer.restoration_end()
