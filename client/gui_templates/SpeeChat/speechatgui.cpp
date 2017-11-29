#include "speechatgui.h"
#include "ui_speechatgui.h"

SpeeChatGUI::SpeeChatGUI(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::SpeeChatGUI)
{
    ui->setupUi(this);
}

SpeeChatGUI::~SpeeChatGUI()
{
    delete ui;
}
