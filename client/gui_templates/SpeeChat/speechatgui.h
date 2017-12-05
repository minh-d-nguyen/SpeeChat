#ifndef SPEECHATGUI_H
#define SPEECHATGUI_H

#include <QMainWindow>

namespace Ui {
class SpeeChatGUI;
}

class SpeeChatGUI : public QMainWindow
{
    Q_OBJECT

public:
    explicit SpeeChatGUI(QWidget *parent = 0);
    ~SpeeChatGUI();

private:
    Ui::SpeeChatGUI *ui;
};

#endif // SPEECHATGUI_H
