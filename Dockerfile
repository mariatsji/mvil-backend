FROM mvilbackend:latest

# create a non-root user to run process as
RUN touch /etc/passwd
RUN touch /etc/group
RUN adduser -D simon
RUN mkdir -p /home/simon/migrations
USER simon
WORKDIR /home/simon

COPY bin/backend-exe .

# not respected by heroku but .. can haz for testing
EXPOSE 8080

COPY migrations/* /home/simon/migrations/

ENV GHCRTS '-T -N1 -qg -qa'

ENTRYPOINT ["/home/simon/backend-exe"]
