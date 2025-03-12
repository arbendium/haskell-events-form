/**
 * Initializes Google Maps component and location field
 */
function initLocationInputOnLoad(mapElement, locationInput, coordinatesInput) {
    const map = new google.maps.Map(mapElement, {
        // Almost whole world on 1080p screen
        center: { lat: 27.68352808378776, lng: 0 },
        zoom: 2,
        mapTypeId: 'roadmap'
    });

    const marker = new google.maps.Marker({
        map,
        animation: google.maps.Animation.DROP,
        draggable: true,
    });

    function setMarker(title, position) {
        marker.setTitle(title);
        marker.setPosition(position);
        const coordStr = `${marker.getPosition().lat()},${marker.getPosition().lng()}`;
        coordinatesInput.value = coordStr;
        marker.addListener('dragend', () => {
            const coordStr = `${marker.getPosition().lat()},${marker.getPosition().lng()}`;
            coordinatesInput.value = coordStr;
            enableSave();
        })
    }

    for (const locationData of eventLocationData) {
        if (locationData.length === 1) {
            const coord = locationData[0];
            if (!coord) {
                continue;
            }
            const [lat, lng] = coord.split(',').map(Number);
            new google.maps.Marker({
                map,
                opacity: 0.7,
                icon: 'https://chart.apis.google.com/chart?chst=d_map_pin_letter&chld= |AAAAAA|000000',
                position: { lat, lng },
            });
        } else if (locationData.length === 4) {
            const [loc, coord, date, eventName] = locationData;
            if (!coord) {
                continue;
            }
            const [lat, lng] = coord.split(',').map(Number);
            const infowindow = new google.maps.InfoWindow({
                content: `<h3>${eventName}</h3><p>${loc}</p><p>${date}</p><p></p>`
            });
            const marker = new google.maps.Marker({
                map,
                position: { lat, lng },
            });
            marker.addListener('click', function () {
                infowindow.open(map, marker);
            });
        }
    }

    // Create the search box and link it to the UI element.
    if (locationInput) {
        const searchBox = new google.maps.places.SearchBox(locationInput);
        searchBox.addListener('places_changed', function () {
            var places = searchBox.getPlaces();
            if (places.length === 0) return;
            const place = places[0];
            if (!place.geometry) return;
            var bounds = new google.maps.LatLngBounds();
            setMarker(place.name, place.geometry.location);
            if (place.geometry.viewport) {
                // Only geocodes have viewport.
                bounds.union(place.geometry.viewport);
            } else {
                bounds.extend(place.geometry.location);
            }
            map.fitBounds(bounds);
        });
    }

    if (typeof currentEventLocationData !== 'undefined' && currentEventLocationData[1]) {
        const [lat, lng] = currentEventLocationData[1].split(',').map(Number);
        setMarker(currentEventLocationData[0], { lat, lng });
    }
}

function initLocationInput(mapElement, locationInput, coordinatesInput) {
    window.addEventListener('load', () => {
        initLocationInputOnLoad(mapElement, locationInput, coordinatesInput);
    });
}
