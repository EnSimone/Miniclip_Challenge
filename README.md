Chat_Server - Miniclip Challenge
=====
Developed by Enrico Simone (EnSimone on GitHub)

How to launch the app:
-----

	1)rebar3 compile
	2)rebar3 release
	3)rebar3 shell
	
-----

How to test the app:
-----
	1)open a linux/windows terminal
	2)connect to the port 8080 (e.g. telnet localhost 8080)
	3)the application will show the steps to follow
	
-----
	
Notes:
-----
	Using telnet might introduce some hard-to-manage characters under specific cases (e.g. deletion and rewriting of charaters before hitting return)
	
-----

Mandatory task:
-----
	1) Erlang project setup (erlang_project_setup on git's commit) - 04/02/2024
	2) Accept multiple TCP/IP client connections at the same time (multiple_connections on git's commit) - 06/02/2024
	3) Handle rooms (rooms on git's commit) - 13/02/2024
	4) AWS: create a EC2 instance (AWS_ec2 on git's commit) - 14/02/2024
	5) AWS: add ASG and ELB (AWS_asg_elb on git's commit) - 14/02/2024
	6) Send private messages (private_messages on git's commit) - 10/02/2024
	7) Handle private rooms (private_room on git's commit) - 13/02/2024
-----