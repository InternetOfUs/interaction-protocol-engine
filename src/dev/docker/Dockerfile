# syntax=docker/dockerfile:experimental

FROM ubuntu:latest

RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y update

# Install java compiling
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install openjdk-11-jdk
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install maven

# Install swi prolog
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install software-properties-common
RUN apt-add-repository ppa:swi-prolog/stable
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y update
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install swi-prolog

# Install utilities
RUN --mount=type=cache,id=apt-ubuntu-cache-amd64,target=/var/cache/apt apt-get -y install less

WORKDIR /app

COPY pom.xml .
RUN --mount=type=cache,id=mvn-cache,target=/root/.m2 mvn dependency:go-offline


CMD ["bash"]