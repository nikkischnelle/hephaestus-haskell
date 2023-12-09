FROM debian:bookworm-slim
RUN apt-get update -y 
RUN apt-get upgrade -y
RUN apt-get install libmagic-dev -y

ENV LANG C.UTF-8
WORKDIR /hephaestus/data
COPY ./hephaestus /hephaestus/
RUN chmod +x /hephaestus/hephaestus
CMD ["/hephaestus/hephaestus"]