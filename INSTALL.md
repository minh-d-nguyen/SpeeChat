SpeeChat Dependency Installation Instructions<br>
for Ubuntu and Mac OS<br>
by: Minh D. Nguyen and Quinn Collins
<br><br>
Dependencies:<br>
Homebrew (For Mac OS only)<br>
Python 2.7<br>
PortAudio<br>
Pyaudio<br>
Erlport<br>
ZeroMQ (zmq) library for Python<br>
Erlzmq<br>
PyQt5<br>
Google's Speech Recognition Package
<br><br>
Homebrew (for Mac OS):<br>
Run:<br>
`$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`<br>
from the terminal.
<br><br>
Python 2.7:<br>
Please refer to the installation instruction at:<br>
https://www.python.org/download/releases/2.7/
<br><br>
PortAudio v19:<br>
Ubuntu: Please refer to http://www.portaudio.com/download.html<br>
Mac OS:<br>
Assuming you have the latest version of Homebrew installed, run the following commands from the terminal:<br>
`$ ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null`<br>
`$ brew install portaudio`
<br><br>
PyAudio (PortAudio v19 Python binding):<br>
Please refer to instructions at:<br>
https://people.csail.mit.edu/hubert/pyaudio/
<br><br>
ErlPort (a library which helps connect Erlang to Python and Ruby):<br>
Please refer to http://erlport.org/downloads/
<br><br>
ZeroMQ (zmq) for Python:<br>
Please refer to: http://zeromq.org/intro:get-the-software
<br><br>
Erlzmq (High-performance NIF based Erlang bindings for the ZeroMQ):<br>
Please refer to: https://github.com/zeromq/erlzmq2
<br><br>
PyQt5:<br>
For Mac OS:<br>
Assuming you have the latest version of Homebrew installed, run the following commands from the terminal:<br>
`$ brew install pyqt5`<br>
For Ubuntu:<br>
Run the following commands from the terminal:<br>
`$ sudo apt-get install python-pyqt5`
<br><br>
Google's Speech Recognition Package:<br>
Assuming you have pip or pip3 installed, run the following commands from the terminal:<br>
`$ sudo pip install SpeechRecognition`
<br><br>
IMPORTANT: In order to run SpeeChat in the client side, the paths to erlport and erlzmq have to be in the environment variable ERL_LIBS, and the paths to zmq, speech_recognition, and PyQt5 have to be in the environment variable PATH or PYTHONPATH. See how to check and set environment variables here:<br>
https://en.wikipedia.org/wiki/Environment_variable
<br><br>
Note that in the process of installing the above packages, depending on the computer, extra packages might also have to be installed.
