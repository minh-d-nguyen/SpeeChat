# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'speechatgui.ui'
#
# Created by: PyQt5 UI code generator 5.9
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

class Ui_SpeeChatGUI(object):
    def setupUi(self, SpeeChatGUI):
        SpeeChatGUI.setObjectName("SpeeChatGUI")
        SpeeChatGUI.resize(400, 355)
        self.centralWidget = QtWidgets.QWidget(SpeeChatGUI)
        self.centralWidget.setObjectName("centralWidget")
        self.ChatDisplay = QtWidgets.QListWidget(self.centralWidget)
        self.ChatDisplay.setGeometry(QtCore.QRect(10, 10, 381, 211))
        self.ChatDisplay.setObjectName("ChatDisplay")
        self.MsgLabel = QtWidgets.QLabel(self.centralWidget)
        self.MsgLabel.setGeometry(QtCore.QRect(10, 240, 91, 16))
        self.MsgLabel.setObjectName("MsgLabel")
        self.MsgEdit = QtWidgets.QTextEdit(self.centralWidget)
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
        self.CancelBtn.setText(_translate("SpeeChatGUI", "Cancel"))

