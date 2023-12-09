FROM ubuntu:latest
RUN apt update
RUN apt install -y file

ENV LANG C.UTF-8
WORKDIR /hephaestus/data
COPY ./hephaestus ../
RUN chmod +x ../hephaestus
CMD ["../hephaestus"]