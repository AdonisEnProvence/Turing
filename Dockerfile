FROM erlang:24-alpine

WORKDIR /turing

ADD . .

CMD ./loop.sh
