#include <SDL2/SDL_events.h>
#include <SDL2/SDL_timer.h>

unsigned int SlickTick(unsigned int interval, void* param) {
    SDL_Event event;
    event.type = SDL_WINDOWEVENT;
    event.window.event = SDL_WINDOWEVENT_EXPOSED;
    SDL_PushEvent(&event);
    return interval;
}

int SlickEventFilter(void *userdata, SDL_Event* event) {
    if (event->type == SDL_WINDOWEVENT || event->type == SDL_KEYDOWN)
        return 1;
    else
        return 0;
}

void SlickSetup() {
    SDL_AddTimer(100, &SlickTick, NULL);
    SDL_SetEventFilter(&SlickEventFilter, NULL);
}

