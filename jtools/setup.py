import os
from setuptools import setup, find_packages

def get_long_description():
    with open(os.path.join(os.path.dirname(__file__), 'README.md')) as f:
        return f.read()

setup(
    # The name of the package. Required. (<200 chars)
    name = 'jtools',
    # It is recommended that versions take the form
    # major.minor[.patch[.sub]]. Required.
    version = __import__('jtools').get_version(),
    # Home page for the package. Required.
    url = 'None',
    # Author is required. Alternatively, a maintainer may be
    # specified using the 'maintainer' and 'maintainer_email'
    # parameters. (<200 chars)
    author = 'Jason Furtney',
    author_email = 'jkfurtney@gmail.com',
    # A short description of the project (<200 chars)
    description = "Utilities",
    # A long description. Can be any length.
    long_description = get_long_description(),
    keywords = 'None',
    # Location where the package may be downloaded.
    # 'download_url': 'DOWNLOAD_URL',
    # Packages and data files included in the project.
    tests_require = ['nose'],
    install_requires = [],
    packages = find_packages(),
    include_package_data = True,
    # A list of strings used by PyPI to classify the project.
    classifiers = [
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 3'
        'Development Status :: 1 - Planning',
        'Operating System :: OS Independent',
        'Intended Audience :: End Users/Desktop',
        'License :: OSI Approved :: MIT License',
    ],
    # You can specify entry points. console_scripts are
    # Python source code intended to be started from the
    # command line.
    entry_points = {
        'console_scripts': [
            'jtools = jtools.main:main',
        ]
    }
)
