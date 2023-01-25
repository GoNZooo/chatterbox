FROM ubuntu:20.04 as build

SHELL ["/bin/bash", "-c"]

RUN apt-get update && \
    apt-get install -y \
           unzip git curl wget gcc make autoconf automake build-essential \
           libncurses5-dev libssl-dev locales

ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8     
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

COPY . /app

WORKDIR /app/server

# Install asdf for version management
RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.11.1 && \
    echo -e '\n. $HOME/.asdf/asdf.sh' >> ~/.bashrc

RUN . $HOME/.asdf/asdf.sh && \
    asdf plugin add erlang && \
    asdf plugin add elixir && \
    asdf plugin add purescript && \
    asdf plugin add spago && \
    asdf plugin add purerl && \
    asdf plugin add rebar && \
    asdf plugin add nodejs && \
    asdf install erlang && \
    asdf global erlang 25.1.2 && \
    asdf install rebar && \
    asdf install purescript && \
    asdf install spago && \
    asdf install purerl && \
    asdf install elixir && \
    asdf install nodejs

RUN . $HOME/.asdf/asdf.sh && npm install --global esbuild

WORKDIR /app/client

RUN . $HOME/.asdf/asdf.sh && spago bundle-app --to ../server/priv/static/app.js

WORKDIR /app/server

RUN . $HOME/.asdf/asdf.sh && \
    mix local.hex --force && \
    mix local.rebar --force && \
    MIX_ENV=prod mix deps.get && \
    MIX_ENV=prod mix compile && \
    MIX_ENV=prod mix release

RUN ls -lsa

# Release image

FROM ubuntu:20.04 as release

RUN apt-get update && apt-get install -y unzip git curl wget linux-libc-dev-amd64-cross locales

COPY --from=build /app/server/_build/prod/rel/chatterbox /app
COPY --from=build /app/server/priv /app/static

RUN ls -lsa /app

WORKDIR /app

CMD ["bin/chatterbox", "start"]
