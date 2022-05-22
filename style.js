var el = document.querySelectorAll('.error');
console.log("efenj")

for (var i = 0; i < el.length; i++) {
    var currentEl = el[i];
    currentEl.style.bottom = i + "rem";
    console.log(el)
}