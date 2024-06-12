# Use the official Haskell image from Docker Hub
FROM haskell:9.6.5 as builder

# Set the working directory in the Docker container
WORKDIR /app

# Copy the stack configuration files and Haskell package descriptor
COPY hserver/stack.yaml hserver/package.yaml hserver/hserver.cabal /app/
COPY hserver/README.md hserver/CHANGELOG.md hserver/LICENSE hserver/Setup.hs /app/

# Copy the rest of the necessary source files
COPY hserver/src /app/src
COPY hserver/app /app/app
COPY hserver/test /app/test
COPY hserver/static /app/static
COPY hserver/projects.json /app/projects.json

# Install dependencies
RUN stack build --system-ghc --dependencies-only

# Build the project
RUN stack install --system-ghc

# Copy the build artifacts to a new image to keep the image size small
FROM ubuntu:latest

WORKDIR /app

# Install libgmp (required by many Haskell applications)
RUN apt-get update && apt-get install -y \
  libgmp-dev \
  && rm -rf /var/lib/apt/lists/*

# Copy the binary from the builder image
# Note: Adjust the path according to where the executable is placed
COPY --from=builder /root/.local/bin/hserver-exe /app/

COPY --from=builder /app/static /app/static

# Expose the port your app runs on
EXPOSE 8080

# Set the command to run your application
CMD ["./hserver-exe"]

