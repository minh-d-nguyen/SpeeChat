Dependency installation instructions for Ubuntu 16.04

dependencies:
python 2.7
portAudio19
pyaudio
google's speech recognition package
erlport
zmq
erlzmq
PyQt5

install:
sudo apt-get install python3-pip
sudo apt-get install portaudio19-dev
sudo apt-get install python-all-dev
sudo apt-get install python-pyqt5
sudo pip3 install pyaudio
sudo pip3 install SpeechRecognition
sudo pip install zmq
git clone https://github.com/hdima/erlport.git
cd erlport/
sudo make
sudo ./release

TODO: Update to Python 2
