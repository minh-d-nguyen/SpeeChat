"""
speechat_gui.py
by the SpeeChat team (Minh D. Nguyen, Quinn Collins, and Arpan Gurung)

This module includes the code for the GUI for the SpeeChat application,
including methods to communicate with the Erlang process through zmq.
NOTE: The zmq communication code is adapted from the example at the website:
http://www.23min.com/2013/03/erlang-to-pyqt-via-0mq/

"""

import os
import sys
import zmq
from erlport.erlang import cast, set_message_handler
from erlport.erlterms import Atom
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import (
    QApplication,
    QMainWindow,
    QMessageBox,
    QAction,
    QPushButton,
    QInputDialog,
)
from PyQt5.QtCore import QThread, QObject, QTimer

class Ui_SpeeChatGUI(object):
    def setupUi(self, SpeeChatGUI):
        """
        Instantiate GUI chat window, buttons, and text input field using PyQt5

        """
        SpeeChatGUI.setObjectName("SpeeChat")
        SpeeChatGUI.resize(400, 355)
        self.centralWidget = QtWidgets.QWidget(SpeeChatGUI)
        self.centralWidget.setObjectName("centralWidget")
        self.ChatDisplay = QtWidgets.QListWidget(self.centralWidget)
        self.ChatDisplay.setGeometry(QtCore.QRect(10, 10, 381, 211))
        self.ChatDisplay.setObjectName("ChatDisplay")
        self.MsgLabel = QtWidgets.QLabel(self.centralWidget)
        self.MsgLabel.setGeometry(QtCore.QRect(10, 240, 91, 16))
        self.MsgLabel.setObjectName("MsgLabel")
        self.MsgEdit = QtWidgets.QLineEdit(self.centralWidget)
        self.MsgEdit.setGeometry(QtCore.QRect(110, 230, 281, 41))
        self.MsgEdit.setObjectName("MsgEdit")
        self.SendBtn = QtWidgets.QPushButton(self.centralWidget)
        self.SendBtn.setGeometry(QtCore.QRect(222, 280, 81, 32))
        self.SendBtn.setObjectName("SendBtn")
        self.CancelBtn = QtWidgets.QPushButton(self.centralWidget)
        self.CancelBtn.setGeometry(QtCore.QRect(310, 280, 81, 32))
        self.CancelBtn.setObjectName("CancelBtn")
        SpeeChatGUI.setCentralWidget(self.centralWidget)
        self.mainToolBar = QtWidgets.QToolBar(SpeeChatGUI)
        self.mainToolBar.setObjectName("mainToolBar")
        SpeeChatGUI.addToolBar(QtCore.Qt.TopToolBarArea, self.mainToolBar)
        self.statusBar = QtWidgets.QStatusBar(SpeeChatGUI)
        self.statusBar.setObjectName("statusBar")
        SpeeChatGUI.setStatusBar(self.statusBar)

        self.retranslateUi(SpeeChatGUI)
        QtCore.QMetaObject.connectSlotsByName(SpeeChatGUI)

    def retranslateUi(self, SpeeChatGUI):
        """
        set text in GUI
        
        """
        _translate = QtCore.QCoreApplication.translate
        SpeeChatGUI.setWindowTitle(_translate("SpeeChatGUI", "SpeeChat"))
        self.MsgLabel.setText(_translate("SpeeChatGUI", "Your message:"))
        self.SendBtn.setText(_translate("SpeeChatGUI", "Send"))
        self.CancelBtn.setText(_translate("SpeeChatGUI", "Quit"))

class ZeroMQ_Listener(QObject):
    message = QtCore.pyqtSignal(str)
    def __init__(self):
        """
        Instantiate socket to send messages to erlang client.
        This class is copied from:
        http://www.23min.com/2013/03/erlang-to-pyqt-via-0mq/

        """
        QObject.__init__(self)
         
        # ZeroMQ endpoint
        context = zmq.Context()
        self.socket = context.socket(zmq.PULL)
        self.socket.connect("tcp://127.0.0.1:5561")
        self.running = True
     
    def loop(self):
        while self.running:
            string = self.socket.recv()
            self.message.emit(string)

class ChatGUI(Ui_SpeeChatGUI, QMainWindow):
    def __init__(self, username, pid, transcript):
        """
        Set up and handle events in GUI.
        
        """
        super(ChatGUI, self).__init__()
        self.setupUi(self)
        self.username = username
        self.pid = pid
        self.all_msgs = reversed(transcript)
        self.setup_components()

        # ZeroMQ hook up listener to Qt
        # This part is copied from:
        # http://www.23min.com/2013/03/erlang-to-pyqt-via-0mq/
        self.thread = QThread()
        self.zeromq_listener = ZeroMQ_Listener()
        self.zeromq_listener.moveToThread(self.thread)
        self.thread.started.connect(self.zeromq_listener.loop)
        self.zeromq_listener.message.connect(self.signal_received)
        self.thread.start()
     
    def signal_received(self, message):
        """
        Handle receiving messages to be displayed in GUI.
        
        """
        # handle speech-to-text messages
        if message[0] == '*':
            message = message[1:]
            curr_msg = str(self.MsgEdit.text())
            self.MsgEdit.setText(curr_msg + " " + message)
            return

        # handle text messages sent from the server
        self.all_msgs.append(message)
        currentCount = len(self.all_msgs)
        while (
            currentCount > 1 and
            self.all_msgs[currentCount - 1] < self.all_msgs[currentCount - 2]
        ):
            temp = self.all_msgs[currentCount - 1]
            self.all_msgs[currentCount - 1] = self.all_msgs[currentCount - 2]
            self.all_msgs[currentCount - 2] = temp
            currentCount -= 1
        self.ChatDisplay.insertItem(currentCount, message)
        self.ChatDisplay.scrollToBottom()
 
    def closeEvent(self, event):
        """
        Handle closing window when the close signal is received.

        """
        try:
            self.zeromq_listener.running = False
            self.thread.terminate()
            cast(self.pid, (Atom("newmsg"), "--quit"))
        except:
            pass

    def setup_components(self):
        """
        Connect button functionality to associated functions; display messages.
        
        """
        self.CancelBtn.clicked.connect(self.exit_chat)
        self.SendBtn.clicked.connect(self.send_msg)
        self.all_msgs = sorted(self.all_msgs, key=lambda x: x[2])
        self.MsgEdit.returnPressed.connect(self.send_msg)
        for msg in self.all_msgs:
            currentRow = self.ChatDisplay.count()
            self.ChatDisplay.insertItem(currentRow, msg)
        self.ChatDisplay.scrollToBottom()
    
    def exit_chat(self):
        """
        Close chat window and associated socket and thread.
        
        """
        self.close()
        try:
            self.zeromq_listener.running = False
            self.thread.terminate()
            cast(self.pid, (Atom("newmsg"), "--quit"))
        except:
            pass

    def send_msg(self):
        """
        Send message to erlang client to be sent to the server.
        
        """
        msg = str(self.MsgEdit.text())
        try:
            cast(self.pid, (Atom("newmsg"), msg))
            self.MsgEdit.clear()
            if msg == "--quit":
                self.exit_chat()
        except:
            pass

def create_gui(username, pid, transcript):
    """
    Instantiate the GUI to run SpeeChat.

    """
    app = QApplication(sys.argv)
    GUI = ChatGUI(username, pid, transcript)
    GUI.show()
    sys.exit(app.exec_())
