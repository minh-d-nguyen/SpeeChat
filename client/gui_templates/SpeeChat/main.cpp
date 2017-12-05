#include "speechatgui.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    SpeeChatGUI w;
    w.show();

    return a.exec();
}
