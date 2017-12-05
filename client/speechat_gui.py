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
        _translate = QtCore.QCoreApplication.translate
        SpeeChatGUI.setWindowTitle(_translate("SpeeChatGUI", "SpeeChatGUI"))
        self.MsgLabel.setText(_translate("SpeeChatGUI", "Your message:"))
        self.SendBtn.setText(_translate("SpeeChatGUI", "Send"))
        self.CancelBtn.setText(_translate("SpeeChatGUI", "Quit"))


class ZeroMQ_Listener(QObject):
    message = QtCore.pyqtSignal(str)
    def __init__(self):
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
        super(ChatGUI, self).__init__()
        self.setupUi(self)
        self.username = username
        self.pid = pid
        self.all_msgs = reversed(transcript)
        self.setup_components()

        # CODE COPIED FROM OTHER SOURCES
        # ZeroMQ hook up listener to Qt
        self.thread = QThread()
        self.zeromq_listener = ZeroMQ_Listener()
        self.zeromq_listener.moveToThread(self.thread)
        self.thread.started.connect(self.zeromq_listener.loop)
        self.zeromq_listener.message.connect(self.signal_received)
        self.thread.start()
     
    def signal_received(self, message):
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
        try:
            self.zeromq_listener.running = False
            self.thread.terminate()
            cast(self.pid, (Atom("newmsg"), "--quit"))
        except:
            pass

    def setup_components(self):
        self.CancelBtn.clicked.connect(self.exit_chat)
        self.SendBtn.clicked.connect(self.send_msg)
        self.all_msgs = sorted(self.all_msgs, key=lambda x: x[2])
        self.MsgEdit.returnPressed.connect(self.send_msg)
        for msg in self.all_msgs:
            currentRow = self.ChatDisplay.count()
            self.ChatDisplay.insertItem(currentRow, msg)
        self.ChatDisplay.scrollToBottom()

    def exit_chat(self):
        self.close()
        try:
            self.zeromq_listener.running = False
            self.thread.terminate()
            cast(self.pid, (Atom("newmsg"), "--quit"))
        except:
            pass

    def send_msg(self):
        msg = str(self.MsgEdit.text())
        # Send msg
        try:
            cast(self.pid, (Atom("newmsg"), msg))
            self.MsgEdit.clear()
            if msg == "--quit":
                self.exit_chat()
        except:
            pass


def create_gui(username, pid, transcript):
    app = QApplication(sys.argv)
    GUI = ChatGUI(username, pid, transcript)
    GUI.show()
    sys.exit(app.exec_())
