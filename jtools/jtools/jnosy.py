#!/usr/bin/env python
"""Watch for changes in a collection of source files. If changes, run
nosetests.
"""
from __future__ import absolute_import
import ConfigParser
import glob
from optparse import OptionParser
import os
import stat
import subprocess
import sys
import time


class Nosy(object):
    """Watch for changes in all source files. If changes, run nosetests.
    """

    def __init__(self):
        """Return an instance with the default configuration, and a
        command line parser.
        """
        self.config = ConfigParser.SafeConfigParser()
        self.config.add_section('nosy')
        self.config.set('nosy', 'base_path', '.')
        self.config.set('nosy', 'glob_patterns', '')
        self.config.set('nosy', 'exclude_patterns', '')
        self.config.set('nosy', 'extra_paths', '')
        self.config.set('nosy', 'options',  '')
        self.config.set('nosy', 'tests', '')
        # paths config retained for backward compatibility; use
        # extra_paths for any files or paths that aren't easily
        # included via base_path, glob_patterns, and exclude_patterns
        self.config.set('nosy', 'paths', '*.py')
        self._build_cmdline_parser()


    def _build_cmdline_parser(self):
        description = 'Automatically run nose whenever source files change.'
        self._opt_parser = OptionParser(description=description)
        defaults = dict(config_file='setup.cfg')
        self._opt_parser.set_defaults(**defaults)
        self._opt_parser.add_option(
            '-c', '--config', action='store', dest='config_file',
            help='configuration file path and name; '
                 'defaults to %(config_file)s' % defaults)


    def parse_cmdline(self):
        """Parse the command line and set the config_file attribute.
        """
        options, args = self._opt_parser.parse_args()
        if len(args) > 0:
            self._opt_parser.error('no arguments allowed')
        self.config_file = options.config_file


    def _read_config(self):
        if self.config_file:
            try:
                self.config.readfp(open(self.config_file, 'rt'))
            except IOError, msg:
                self._opt_parser.error("can't read config file:\n %s" % msg)
        try:
            self.base_path = self.config.get('nosy', 'base_path')
            self.glob_patterns = self.config.get(
                'nosy', 'glob_patterns').split()
            self.exclude_patterns = self.config.get(
                'nosy', 'exclude_patterns').split()
            self.extra_paths = self.config.get('nosy', 'extra_paths').split()
            self.nose_opts = self.config.get('nosy', 'options')
            self.nose_args = self.config.get('nosy', 'tests')
            # paths config retained for backward compatibility; use
            # extra_paths for any files or paths that aren't easily
            # included via base_path, glob_patterns, and
            # exclude_patterns
            self.paths = self.config.get('nosy', 'paths').split()
        except ConfigParser.NoSectionError:
            self._opt_parser.error("nosy section not found in config file")
            sys.exit(1)
        except ConfigParser.NoOptionError:
            # Use default(s) from __init__()
            pass


    def _calc_extra_paths_checksum(self):
        """Return the checksum for the files given by the extra paths
        pattern(s).

        self.paths is included for backward compatibility.
        """
        checksum = 0
        for path in self.extra_paths + self.paths:
            for file_path in glob.iglob(path):
                stats = os.stat(file_path)
                checksum += stats[stat.ST_SIZE] + stats[stat.ST_MTIME]
        return checksum


    def _calc_exclusions(self, root):
        """Return a set of file paths to be excluded from the checksum
        calculation.
        """
        exclusions = set()
        for pattern in self.exclude_patterns:
            for file_path in glob.iglob(os.path.join(root, pattern)):
                exclusions.add(file_path)
        return exclusions


    def _calc_dir_checksum(self, exclusions, root):
        """Return the checksum for the monitored files in the
        specified directory tree.
        """
        checksum = 0
        for pattern in self.glob_patterns:
            for file_path in glob.iglob(os.path.join(root, pattern)):
                if file_path not in exclusions:
                    stats = os.stat(file_path)
                    checksum += stats[stat.ST_SIZE] + stats[stat.ST_MTIME]
        return checksum


    def _checksum(self):
        """Return a checksum which indicates if any files in the paths
        list have changed.
        """
        checksum = self._calc_extra_paths_checksum()
        for root, dirs, files in os.walk(self.base_path):
            exclusions = self._calc_exclusions(root)
            checksum += self._calc_dir_checksum(exclusions, root)
        return checksum


    def run(self):
        """Run nose whenever the source files (default ./*.py) change.

        Re-read the configuration before each nose run so that options
        and arguments may be changed.
        """
        checksum = 0
        self._read_config()
        while True:
            if self._checksum() != checksum:
                self._read_config()
                checksum = self._checksum()
                time_format = "%d-%b-%y %I:%M %p"
                print "nosy running tests", time.strftime(time_format)
                subprocess.call(
                    ['nosetests']
                    + self.nose_opts.replace('\\\n', '').split()
                    + self.nose_args.replace('\\\n', '').split())
                print "nosy idle", time.strftime(time_format)
            time.sleep(1)


def main():
    nosy = Nosy()
    nosy.parse_cmdline()
    try:
        nosy.run()
    except KeyboardInterrupt:
        sys.exit(130)
    except SystemExit:
        sys.exit(0)


if __name__ == '__main__':
    main()
