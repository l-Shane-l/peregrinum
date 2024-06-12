#!/usr/bin/perl
use strict;
use warnings;
use JSON;
use File::Slurp;
use Getopt::Long;

# Command line argument parsing
my $new_project_name;
GetOptions("name=s" => \$new_project_name) or die "Usage: $0 --name PROJECT_NAME\n";

# Check if the project name was provided
die "You must provide a project name with --name.\n" unless defined $new_project_name;

# Path to the JSON file
my $file_path = 'projects.json';

# Read the existing JSON file
my $json_text = read_file($file_path);
my $json = decode_json($json_text);

# Define the new project entry using the provided project name
my $new_project = {
    name => $new_project_name,
    icon => "fas fa-some-icon text-blue-800 text-5xl", # Placeholder icon, consider updating this per project basis
    summary => {
        title => $new_project_name,
        overview => "A short summary about $new_project_name, highlighting what visitors can learn or discover.",
        additionalInfo => "",
        imageSrc => "/assets/gifs/$new_project_name.gif"
    },
    details => {
        title => $new_project_name,
        overview => "$new_project_name began as an innovative project and has evolved to incorporate advanced technologies and methodologies, demonstrating significant progress in its field.",
        additionalInfo => "More developments and updates about $new_project_name are on the way as the project continues to expand its capabilities.",
        imageSrc => "/assets/gifs/$new_project_name.gif"
    }
};

# Add the new project to the existing array of projects
push @{$json->{projects}}, $new_project;

# Encode the updated JSON
my $updated_json_text = encode_json($json);

# Write the updated JSON back to the file
write_file($file_path, $updated_json_text);

print "New project '$new_project_name' added successfully.\n";
