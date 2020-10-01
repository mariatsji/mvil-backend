FROM mvilbackend:latest

# create a non-root user to run process as
RUN touch /etc/passwd
RUN touch /etc/group
RUN adduser -D simon
USER simon
WORKDIR /home/simon

COPY bin/backend-exe .

EXPOSE 8080

ENV GHCRTS '-T -N1 -qg -qa'

ENTRYPOINT ["/home/simon/backend-exe"]
