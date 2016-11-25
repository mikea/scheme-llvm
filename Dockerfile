FROM debian:testing-slim
MAINTAINER mike.aizatsky@gmail.com
RUN apt-get update
RUN apt-get install -y build-essential clang rake chicken-bin libchicken-dev llvm
RUN chicken-install test r7rs

RUN mkdir /src /build /out
COPY . /src/
WORKDIR /src/
CMD rake