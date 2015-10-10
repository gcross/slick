#include <cassert>
#include <cairo.h>
#include <glib-object.h>
#include <iostream>
#include <librsvg/rsvg.h>
#include <SDL2/SDL.h>

GError* error;

struct SDL {
    SDL() { SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER); }
    ~SDL() { SDL_Quit(); }
};

struct SDL_Window_ {
    SDL_Window *_;

private:
    bool is_full_screen;
    int window_x, window_y, window_width, window_height;

public:
    SDL_Window_(int window_width, int window_height) :
        _(SDL_CreateWindow(
            "Hello, world!",
            SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
            window_width, window_height,
            SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE
        )),
        is_full_screen(false),
        window_x(-1),
        window_y(-1),
        window_width(-1),
        window_height(-1)
    {}

    ~SDL_Window_() { SDL_DestroyWindow(_); }

    void ToggleFullScreen() {
        if (is_full_screen) {
            assert(window_x >= 0);
            assert(window_y >= 0);
            assert(window_width > 0);
            assert(window_height > 0);
            SDL_SetWindowFullscreen(_, 0);
            SDL_SetWindowSize(_, window_width, window_height);
            SDL_SetWindowPosition(_, window_x, window_y);
            window_x = -1;
            window_y = -1;
            window_width = -1;
            window_height = -1;
        } else {
            SDL_GetWindowPosition(_, &window_x, &window_y);
            SDL_GetWindowSize(_, &window_width, &window_height);
            SDL_DisplayMode mode;
            SDL_GetCurrentDisplayMode(0, &mode);
            SDL_SetWindowSize(_, mode.w, mode.h);
            SDL_SetWindowFullscreen(_, SDL_WINDOW_FULLSCREEN);
        }
        is_full_screen = not is_full_screen;
    }
};

struct SDL_Renderer_ {
    SDL_Renderer* _;
    SDL_Renderer_(SDL_Window *window)
        : _(SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED)) {}
    ~SDL_Renderer_() { SDL_DestroyRenderer(_); }
};

struct RsvgHandle_ {
    RsvgHandle *_;
    RsvgHandle_() : _(rsvg_handle_new()) {}
    ~RsvgHandle_() { g_object_unref(_); }
};

struct CairoImageSurface {
    cairo_surface_t *_;
    CairoImageSurface(int width, int height) :
        _(cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height))
    {}
    ~CairoImageSurface() { cairo_surface_destroy(_); }
};

struct CairoContext {
    cairo_t *_;
    CairoContext(cairo_surface_t* surface) : _(cairo_create(surface)) {}
    ~CairoContext() { cairo_destroy(_); }
};

struct SDL_SurfaceFromData_ {
    SDL_Surface* _;
    SDL_SurfaceFromData_(void* pixels, int width, int height)
        : _(SDL_CreateRGBSurfaceFrom(pixels,width,height,32,4*width,0,0,0,0)) {}
    ~SDL_SurfaceFromData_() { SDL_FreeSurface(_); }
};

struct SDL_Texture_ {
    SDL_Texture *_;
    SDL_Texture_(SDL_Renderer* renderer, SDL_Surface *surface)
        : _(SDL_CreateTextureFromSurface(renderer,surface)) {}
    ~SDL_Texture_() { SDL_DestroyTexture(_); }
};

void write_error_and_quit(GError *error) {
    std::cerr << error->message << std::endl;
    abort();
}

void fix_size(double correct_aspect_ratio, int new_width, int new_height, int &fixed_width, int &fixed_height) {
    double new_aspect_ratio = (double)new_width / (double)new_height;
    if(new_aspect_ratio > correct_aspect_ratio) {
        fixed_width = new_width / new_aspect_ratio * correct_aspect_ratio;
        fixed_height = new_height;
    } else {
        fixed_width = new_width;
        fixed_height = new_height * new_aspect_ratio / correct_aspect_ratio;
    }
}

extern "C" {

void slick_write_to_handle(RsvgHandle *handle, unsigned char *buf, unsigned long count) {
    if(not rsvg_handle_write(handle, buf, count, &error)) write_error_and_quit(error);
}

void slick_write_document(void *slick_state, double scale, RsvgHandle* handle);
void slick_toggle_mode(void *slick_state);

int slick_run(const int initial_width, const int initial_height, void *slick_state) {
    int width = initial_width, height = initial_height;
    double aspect_ratio = (double)width / (double)height;
    SDL sdl;
    SDL_Window_ window(width,height);
    // window.ToggleFullScreen();
    SDL_Renderer_ renderer(window._);
    SDL_SetWindowBordered(window._, SDL_TRUE);
    while(true) {
        CairoImageSurface cairo_surface(width, height);
        CairoContext context(cairo_surface._);
        cairo_set_source_rgb(context._, 1, 1, 1);
        cairo_paint(context._);
        RsvgHandle_ handle;
        slick_write_document(slick_state, (double)width/(double)initial_width, handle._);
        GError *error;
        if(not rsvg_handle_close(handle._, &error)) write_error_and_quit(error);
        rsvg_handle_render_cairo(handle._, context._);
        SDL_SurfaceFromData_ sdl_surface(cairo_image_surface_get_data(cairo_surface._), width, height);
        SDL_Texture_ texture(renderer._, sdl_surface._);
        SDL_RenderCopy(renderer._, texture._, NULL, NULL);
        SDL_RenderPresent(renderer._);

        SDL_Event event;
        SDL_PollEvent(&event);
        switch(event.type) {
            case SDL_QUIT: return 0;
            case SDL_WINDOWEVENT:
                switch(event.window.event) {
                    case SDL_WINDOWEVENT_CLOSE: return 0;
                    case SDL_WINDOWEVENT_RESIZED:
                    case SDL_WINDOWEVENT_MAXIMIZED:
                    case SDL_WINDOWEVENT_RESTORED:
                        fix_size(aspect_ratio,event.window.data1,event.window.data2,width,height);
                        SDL_SetWindowSize(window._,width,height);
                        break;
                }
                break;
            case SDL_KEYDOWN:
                switch(event.key.keysym.sym) {
                    case SDLK_ESCAPE: return 0;
                    case SDLK_SPACE: slick_toggle_mode(slick_state); break;
                    case SDLK_f: window.ToggleFullScreen(); break;
                }
        }
    }
}

}
