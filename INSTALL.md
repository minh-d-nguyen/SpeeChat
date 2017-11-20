Dependency installation instructions for Ubuntu 16.04

dependencies:
python3
portAudio19
pyaudio
google's speech recognition package
erlport

install:
sudo apt-get install python3-pip
sudo apt-get install portaudio19-dev
sudo apt-get install python-all-dev
sudo pip3 install pyaudio
sudo pop3 install SpeechRecognition
git clone https://github.com/hdima/erlport.git
cd erlport/
sudo make
sudo ./release
