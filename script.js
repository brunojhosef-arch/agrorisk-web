document.addEventListener("DOMContentLoaded", () => {
    // --- Scroll Animations (Intersection Observer) ---
    const observerOptions = {
        root: null,
        rootMargin: '0px',
        threshold: 0.15
    };

    const animateObserver = new IntersectionObserver((entries, observer) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('visible');
                // Don't unobserve to allow re-animating when switching tabs if needed,
                // but for cards, animating once is usually better. Let's unobserve for performance.
                observer.unobserve(entry.target); 
            }
        });
    }, observerOptions);

    function initScrollAnimations() {
        document.querySelectorAll('.animate-up, .scroll-animate').forEach(element => {
            element.classList.remove('visible'); // Reset
            animateObserver.observe(element);
        });
    }

    // Initialize on load
    initScrollAnimations();

    // --- Tab Switching Logic ---
    const tabBtns = document.querySelectorAll('.tab-btn');
    const tabContents = document.querySelectorAll('.tab-content');

    // Add 'show' class to initially active tab for transition
    const initialActive = document.querySelector('.tab-content.active');
    if (initialActive) {
        setTimeout(() => {
            initialActive.classList.add('show');
        }, 50);
    }

    tabBtns.forEach(btn => {
        btn.addEventListener('click', () => {
            // Remove active from all buttons
            tabBtns.forEach(b => b.classList.remove('active'));
            // Add active to clicked button
            btn.classList.add('active');

            const targetId = btn.getAttribute('data-target');

            // Hide all contents with transition
            tabContents.forEach(content => {
                content.classList.remove('show');
                
                // Wait for fade out to complete before display:none
                setTimeout(() => {
                    content.classList.remove('active');
                    
                    if (content.id === targetId) {
                        content.classList.add('active');
                        // Small delay to allow display block to apply before opacity transition
                        setTimeout(() => {
                            content.classList.add('show');
                            // Re-trigger scroll animations for the new active tab
                            initScrollAnimations();
                        }, 50);
                    }
                }, 300); // matches CSS transition duration
            });
        });
    });

    // --- 3D Hover Effect for the Presentation Image ---
    const imageWrapper = document.querySelector('.image-wrapper');
    if (imageWrapper) {
        imageWrapper.addEventListener('mousemove', (e) => {
            const rect = imageWrapper.getBoundingClientRect();
            const x = e.clientX - rect.left; // x position within the element.
            const y = e.clientY - rect.top;  // y position within the element.
            
            const centerX = rect.width / 2;
            const centerY = rect.height / 2;
            
            const rotateX = ((y - centerY) / centerY) * -5; // max 5 deg
            const rotateY = ((x - centerX) / centerX) * 5;  // max 5 deg
            
            imageWrapper.style.transform = `perspective(1000px) rotateX(${rotateX}deg) rotateY(${rotateY}deg) scale3d(1.02, 1.02, 1.02)`;
        });

        imageWrapper.addEventListener('mouseleave', () => {
            imageWrapper.style.transform = `perspective(1000px) rotateX(0) rotateY(0) scale3d(1, 1, 1)`;
        });
    }
});
