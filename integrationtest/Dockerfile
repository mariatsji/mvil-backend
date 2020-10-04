FROM postgres:9.5

ENV POSTGRES_USER posts
ENV POSTGRES_PASSWORD apekatt
ENV POSTGRES_DB posts

COPY db-init.sql /docker-entrypoint-initdb.d/
