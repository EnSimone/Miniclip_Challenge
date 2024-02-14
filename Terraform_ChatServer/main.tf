terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.16"
    }
  }

  required_version = ">= 1.2.0"
}

provider "aws" {
  region = "eu-central-1"
}

resource "aws_instance" "Chat_Server_Instance" {
  ami           = "ami-0faab6bdbac9486fb"
  instance_type = "t2.micro"

  tags = {
    Name = "Chat_Server_Instance"
  }
  
  vpc_security_group_ids = [aws_security_group.allow_ssh.id, aws_security_group.allow_8080.id]

  user_data = <<-EOF
              #!/bin/bash
              sudo apt update
              sudo apt install -y apache2
              sudo ufw allow 8080
              sudo systemctl start apache2
              sudo systemctl enable apache2
              EOF
}

resource "aws_security_group" "allow_ssh" {
  name        = "allow_ssh"
  description = "Allow inbound SSH traffic on port 22"
  
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_8080" {
  name        = "allow_8080"
  description = "Allow inbound traffic on port 8080"
  
  ingress {
    from_port   = 8080
    to_port     = 8080
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_launch_configuration" "Chat_Server_launch_config" {
  name = "Chat_Server-launch-config"

  image_id = "ami-0faab6bdbac9486fb"

  instance_type        = "t2.micro"
  security_groups      = [aws_security_group.allow_ssh.id, aws_security_group.allow_8080.id]
  user_data            = <<-EOF
                          #!/bin/bash
                          sudo apt update
                          sudo apt install -y apache2
                          sudo ufw allow 8080
                          sudo systemctl start apache2
                          sudo systemctl enable apache2
                          EOF
  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_autoscaling_group" "Chat_Server_asg" {
  desired_capacity     = 2
  max_size             = 3
  min_size             = 1
  launch_configuration = aws_launch_configuration.Chat_Server_launch_config.id
  vpc_zone_identifier  = ["subnet-0d230af68c17a0214","subnet-0e60efd49c1583d69","subnet-086a85a4da752c747"]
}

resource "aws_lb" "Chat_Server_elb" {
  name               = "ChatServerElb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.allow_8080.id]
  subnets            = ["subnet-0d230af68c17a0214","subnet-0e60efd49c1583d69","subnet-086a85a4da752c747"]

  enable_deletion_protection = false
  enable_cross_zone_load_balancing = true
  enable_http2 = true
}

resource "aws_lb_listener" "Chat_Server_listener" {
  load_balancer_arn = aws_lb.Chat_Server_elb.arn
  port              = 80
  protocol          = "HTTP"

  default_action {
    target_group_arn = aws_lb_target_group.Chat_Server_target_group.arn
    type             = "forward"
  }
}

resource "aws_lb_target_group" "Chat_Server_target_group" {
  name        = "ChatServerTargetGroup"
  port        = 8080
  protocol    = "HTTP"
  vpc_id      = "vpc-02d36f244ee99e677"
  target_type = "instance"
}

resource "aws_lb_listener_rule" "Chat_Server_listener_rule" {
  listener_arn = aws_lb_listener.Chat_Server_listener.arn

  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.Chat_Server_target_group.arn
  }

  condition {
    path_pattern {
      values = ["/"]
    }
  }
}