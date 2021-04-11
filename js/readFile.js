/*  a classic function: "read"
    read file needs anchor : <input type="file" id="fileInput"> */

document.getElementById('fileInput').addEventListener('change', function selectedFileChanged() {
      console.log(this.files); // will contain information about the file that was selected.
    });

const n = 2 ;

document.getElementById('fileInput').addEventListener('change', function selectedFileChanged() {
  if (this.files.length === 0) {
    console.log('No file selected.');
    return;
  }

  const reader = new FileReader();
  reader.onload = function fileReadCompleted() {
    // when the reader is done, the content is in reader.result.
    const karn = (reader.result).split("\n");
    document.getElementById("zoe").innerHTML = karn;
  };
  reader.readAsText(this.files[0]);
});
