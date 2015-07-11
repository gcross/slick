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
    SDL_Window_(int width, int height) :
        _(SDL_CreateWindow(
            "Hello, world!",
            SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
            width, height,
            SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE
        ))
    {}
    ~SDL_Window_() { SDL_DestroyWindow(_); }
};

struct SDL_Renderer_ {
    SDL_Renderer* _;
    SDL_Renderer_(SDL_Window *window)
        : _(SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED)) {}
    ~SDL_Renderer_() { SDL_DestroyRenderer(_); }
};

struct RsvgHandle_ {
    RsvgHandle *_;
    RsvgHandle_() : _(rsvg_handle_new_from_file("quantum-mechanic.svg",&error)) {
        if(_ == NULL) {
            std::cerr << "Error reading file." << std::endl;
            abort();
        }
    }
    ~RsvgHandle_() { g_object_unref (_); }
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

const int WIDTH = 1024;
const int HEIGHT = 576;

int main() {
    SDL sdl;
    SDL_Window_ window(WIDTH, HEIGHT);
    SDL_Renderer_ renderer(window._);
    SDL_SetWindowBordered(window._, SDL_TRUE);
    RsvgHandle_ handle;
    CairoImageSurface cairo_surface(WIDTH, HEIGHT);
    CairoContext context(cairo_surface._);
    cairo_set_source_rgb(context._, 1, 1, 1);
    cairo_paint(context._);
    rsvg_handle_render_cairo(handle._, context._);
    SDL_SurfaceFromData_ sdl_surface(cairo_image_surface_get_data(cairo_surface._), WIDTH, HEIGHT);
    SDL_Texture_ texture(renderer._, sdl_surface._);
    SDL_RenderCopy(renderer._, texture._, NULL, NULL);
    SDL_RenderPresent(renderer._);
    for(;;) {}
}