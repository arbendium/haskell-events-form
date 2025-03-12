document.querySelector('#resend-email').addEventListener('click', () => {
	fetch(encodeURIComponent(window.eventUuid) + '/resend-email?token=' + encodeURIComponent(window.eventToken), {
		method: 'POST'
	}).then(response => {
		if (response.ok) {
			$.notify({ message: 'Email has been resent' }, { type: 'success' });
		} else {
			$.notify({ message: 'Failed to resend invitation' }, { type: 'danger' });
		}
	}, e => {
		$.notify({ message: 'Error resending email' }, { type: 'danger' });
	});
});
