#!/usr/bin/perl
use strict;
use warnings;
use File::Copy;

# Ask user for the project name
print "Enter the project name: ";
my $project_name = <STDIN>;
chomp $project_name;

# Define file names based on project name
my $file_html = "${project_name}.html";
my $file_summary_html = "${project_name}-summary.html";

# Template content for the main file
my $content_html = <<"END_HTML";
<div id="${project_name}-details" class="bg-white shadow-lg rounded-lg p-6">
  <h2 class="text-xl font-semibold">${project_name}</h2>
  <div class="flex flex-wrap mt-4">
    <div class="w-full md:w-1/2">
      <img
        src="/assets/gifs/${project_name}.gif"
        alt="${project_name} project in action"
        class="w-full h-auto"
      />
    </div>
    <div class="w-full md:w-1/2 px-4">
      <h3 class="text-lg font-bold">${project_name} Overview</h3>
      <p>
        This project began in college as a wearable sensor and has evolved into
        a non-contact version using MIT research principles. It incorporates
        OpenCV in a multithreaded environment, blending digital signal
        processing with linear algebra.
      </p>
      <p>
        Recently, the use of supervised learning has improved system
        performance, making it more efficient in noise handling. Efforts are
        ongoing to prepare ${project_name} for open-sourcing.
      </p>

      <br />
      <button
        hx-get="${project_name}-summary.html"
        hx-target="#${project_name}-details"
        hx-trigger="click"
        hx-swap="outerHTML"
        class="text-blue-500"
      >
        Show Less
      </button>
    </div>
  </div>
</div>
END_HTML

# Template content for the summary file
my $content_summary_html = <<"END_SUMMARY_HTML";
<div
  id="${project_name}-details"
  class="bg-white shadow-lg rounded-lg p-6 flex items-center justify-between"
>
  <div>
    <h2 class="text-xl font-semibold">${project_name}</h2>
    <p class="text-gray-600">
      A short summary about ${project_name}, highlighting what visitors can learn or
      discover.
    </p>
  </div>
  <button
    hx-get="${project_name}.html"
    hx-target="#${project_name}-details"
    hx-trigger="click"
    hx-swap="outerHTML"
    class="text-blue-500 hover:text-blue-700"
  >
    Learn More
  </button>
</div>
END_SUMMARY_HTML

# Write the contents to files
open my $fh, '>', $file_html or die "Cannot open $file_html: $!";
print $fh $content_html;
close $fh;

open $fh, '>', $file_summary_html or die "Cannot open $file_summary_html: $!";
print $fh $content_summary_html;
close $fh;

print "Files created successfully: $file_html, $file_summary_html\n";

