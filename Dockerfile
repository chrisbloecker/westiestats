FROM debian:jessie

# install the stuff that Haskell needs
RUN apt-get update && \
    apt-get install -y libgmp10 && \
    apt-get clean autoclean

# add files
RUN mkdir /westiestats
ADD westiestats /westiestats/
ADD docker-entrypoint.sh /westiestats/
ADD config /westiestats/config/
ADD static /westiestats/static/

WORKDIR /westiestats/

# we will mount the data here
VOLUME ["/westiestats/data/"]

# set the approot
ENV APPROOT http://westiestats.net

EXPOSE 80

# start the app
CMD ["/westiestats/docker-entrypoint.sh"]
