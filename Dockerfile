# Build stage 0
FROM erlang:alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

#Copy src, include folders and rebar.config
COPY src src/
COPY include include/
COPY rebar.config .

#Build the release
RUN rebar3 release


FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
     apk add --no-cache libstdc++ && \
      apk add --no-cache libgcc

# Install the released application
COPY --from=0 buildroot/_build/default/rel/reengineering /reengineering

CMD ["/reengineering/bin/reengineering", "foreground"]
