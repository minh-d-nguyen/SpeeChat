SpeeChat Dependency Installation Instructions
for Ubuntu and Mac OS
by: Minh D. Nguyen and Quinn Collins

Dependencies:
Homebrew (For Mac OS only)
Python 2.7
PortAudio
Pyaudio
Erlport
ZeroMQ (zmq) library for Python
Erlzmq
PyQt5
Google's Speech Recognition Package

Homebrew (for Mac OS):
Run:
$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
from the terminal.

Python 2.7:
Please refer to the installation instruction at:
https://www.python.org/download/releases/2.7/

PortAudio v19:
Ubuntu: Please refer to http://www.portaudio.com/download.html
Mac OS:
Assuming you have the latest version of Homebrew installed, run the following commands from the terminal:
$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null
$ brew install portaudio

PyAudio (PortAudio v19 Python binding):
Please refer to instructions at:
https://people.csail.mit.edu/hubert/pyaudio/

ErlPort (a library which helps connect Erlang to Python and Ruby):
Please refer to http://erlport.org/downloads/

ZeroMQ (zmq) for Python:
Please refer to: http://zeromq.org/intro:get-the-software

Erlzmq (High-performance NIF based Erlang bindings for the ZeroMQ):
Please refer to: https://github.com/zeromq/erlzmq2

PyQt5:
For Mac OS:
Assuming you have the latest version of Homebrew installed, run the following commands from the terminal:
$ brew install pyqt5
For Ubuntu:
Run the following commands from the terminal:
$ apt-get install python-pyqt5

Google's Speech Recognition Package:
Assuming you have pip or pip3 installed, run the following commands from the terminal:
$ sudo pip install SpeechRecognition

IMPORTANT: In order to run SpeeChat in the client side, the paths to erlport and erlzmq have to be in the environment variable ERL_LIBS, and the path to zmq, speech_recognition, and PyQt5 have to be in the environment variable PATH or PYTHONPATH. See how to check and set environment variables here:
https://en.wikipedia.org/wiki/Environment_variable

Note that in the process of installing the above packages, depending on the computer, extra packages might also have to be installed.
