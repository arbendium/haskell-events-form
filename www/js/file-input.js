function initFileInput(input, state) {
	const parent = input.parentElement;
	const label = input.nextElementSibling;
	input.addEventListener('change', () => {
		const files = Object.values(input.files);
		state.push(...files);
		this.value = '';

		files.forEach(file => {
			const p = document.createElement('p');
			p.className = 'file-item to-upload';
			p.innerText = file.name;

			const removeBtn = document.createElement('a');
			removeBtn.className = 'remove';
			removeBtn.href = 'javascript:void(0)';
			removeBtn.innerText = 'Remove';

			removeBtn.addEventListener('click', e => {
				const fileIndex = state.indexOf(file);
				if (fileIndex > -1) {
					state.splice(fileIndex, 1);
				}
				parent.removeChild(e.target.parentElement);
				enableSave();
			});

			p.appendChild(removeBtn);

			parent.insertBefore(p, input);
		});
	});
}
