# SpeeChat

I. CONTRIBUTORS
<br><br>
Arpan Gurung<br>
Minh Nguyen<br>
Quinn Collins
<br><br><br>
II. DESCRIPTION
<br><br>
SpeeChat is a distributed chat application with an option to provide input through speech in addtition to the ordinary typed input. It is implemented using Erlang and Python, along with the use of multiple frameworks supported by these two languages.
<br><br><br>
III. INSTALL INSTRUCTIONS:
<br><br>
Get source code from the Git repository at:<br>
https://github.com/minh-d-nguyen/SpeeChat<br>
Follow the installation instruction in INSTALL.md to install all the dependencies.<br>
After having all the dependencies installed, we run the application by following the steps below.
<br><br><br>
IV. RUN INSTRUCTIONS:
<br><br>
1. Server:<br>
- Start erlang. For running a distributed system, set the long name and cookie<br>
    e.g.    `erl -name ‘server@test’ -setcookie nohackpls`<br>
- Compile chat_server.erl<br>
    e.g.    `c(chat_server).`<br>
- Start the server and specify the room name<br>
    e.g.    `chat_server:start_link(“room”)`
<br><br>
2. Client:<br>
- Start erlang. For running a distributed system, set the long name and cookie (cookie must match the server’s cookie)<br>
    e.g.    `erl -name ‘client@test’ -setcookie nohackpls`<br>
- Compile chat_client.erl<br>
    e.g.    `c(chat_client).`<br>
- Ping the server, wait for “pong” response. If you get “pang”, something is misconfigured, or your wifi/internet might be configured to block direct communication between users<br>
    e.g.    `net_adm:ping(‘server@test’).`<br>
- Start the client<br>
    e.g.    `chat_client:join_room(“room”, user_1).`
<br><br>
- You’re all set to go! A chat GUI window should open. You can either type messages or speak, and your speech will be added as text in the typing box. Sometimes it takes a while to process speech, so don’t expect an immediate response.
<br><br><br>
V. IMPLEMENTATION TECHNOLOGY AND PACKAGES
<br><br>
Server and Client: Erlang<br>
Speech-To-Text: Google Speech-to-Text via Python<br>
Connecting Python speech-to-text and PyQt GUI to Erlang client: Erlport<br>
GUI: PyQt5<br>
Communication (message passing) between Erlang client and PyQt GUI: ZeroMQ and Erlzmq
<br><br><br>
VI. CODE OVERVIEW
<br><br>
Client Code:<br>
1. chat_client.erl:<br>
This file contains the erlang client side chat code. The server code for this client is contained in chat_server.erl. It contains the three main chat client functions, get_message, get_speech, and send_message, and the join_room function that initializes the client. The get_speech function uses erlport to call the get_speech python function in get_speech.py. Furthermore, it exhibits gen_server behaviour for setting up erlzmq and communicating with the GUI. The GUI is contained in speechat_gui.py.<br>
2. get_speech.py:<br>
This file contains the get_speech python function that uses Google’s Speech Recognition to get speech input from the user which is then converted to text. It is called from the chat client in chat_client.erl.<br>
3. speechat_gui.py:<br>
This file contains the python GUI code written using PyQt, a python wrapper for writing graphical user interfaces. Furthermore, it uses erlzmq and erlport to communicate with the chat client code in chat_client.erl.<br>
<br>
Server Code:<br>
1. chat_server.erl:<br>
This file contains the Speechat erlang server code for a chat room. It utilizes gen_server behaviour to not only keep connected process IDs, the chat room transcript, and subscribed usernames as the server state, but also to handle calls to subscribe and unsubscribe, and a cast to send messages to connected clients. The corresponding client code is contained in chat_client.erl.<br>