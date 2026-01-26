import math
import sys
from PIL import Image, ImageDraw, ImageChops

def crop_content_to_transparency(input_path, output_path, tolerance=30):
    img = Image.open(input_path).convert("RGBA")
    width, height = img.size
    
    # 1. Determine Background Color from corners
    # Sample all 4 corners
    corners = [(0, 0), (width-1, 0), (0, height-1), (width-1, height-1)]
    bg_candidates = [img.getpixel(c) for c in corners]
    
    # Assume the most common color is BG (simplistic but usually works)
    # create a difference image against the first corner color
    # (assuming solid background)
    bg_color = bg_candidates[0]
    
    # Create a mask of "non-background" pixels based on color difference
    # We essentially want: diff(pixel, bg_color) > tolerance
    
    # Create a solid image of the bg color
    bg_img = Image.new("RGBA", img.size, bg_color)
    diff = ImageChops.difference(img, bg_img)
    diff = list(diff.getdata()) # List of (r,g,b,a) tuples
    
    # Find bounding box of pixels that are significantly different
    # Simple magnitude check: r+g+b > tolerance
    
    left, top, right, bottom = width, height, 0, 0
    found_content = False
    
    # OPTIMIZATION: Instead of iterating all pixels in python (slow), 
    # use PIL's native getbbox on a thresholded difference image.
    
    # Convert difference to grayscale 'L'
    diff_img = ImageChops.difference(img, bg_img).convert("L")
    
    # Threshold: pixels < tolerance become 0 (black/bg), others become 255 (white/fg)
    # The 'point' method is fast.
    mask = diff_img.point(lambda p: 255 if p > tolerance else 0)
    
    # Get bounding box of the content
    bbox = mask.getbbox()
    
    if not bbox:
        print("Could not detect any content different from background.")
        return

    left, top, right, bottom = bbox
    content_width = right - left
    content_height = bottom - top
    cx = (left + right) / 2
    cy = (top + bottom) / 2
    
    print(f"Detected content box: {bbox}, W={content_width}, H={content_height}")
    
    # 2. Determine Hexagon Orientation
    # Aspect Ratio = H / W
    aspect = content_height / content_width
    
    # Pointy Topped (Standard R sticker):
    # Height = 2R, Width = sqrt(3)R approx 1.732R
    # Ratio H/W = 2/1.732 = 1.1547
    
    # Flat Topped:
    # Height = 1.732R, Width = 2R
    # Ratio H/W = 1.732/2 = 0.866
    
    print(f"Aspect Ratio: {aspect:.4f}")
    
    # Decide orientation
    # We will fit the hexagon to the bounding box.
    
    # Supersampling for anti-aliasing
    scale = 4
    mask_width = width * scale
    mask_height = height * scale
    mask_img = Image.new("L", (mask_width, mask_height), 0)
    draw = ImageDraw.Draw(mask_img)
    
    vertices = []
    
    cx_s = cx * scale
    cy_s = cy * scale
    
    # Shrink to remove edge artifacts/white shell
    # 5 pixels at original scale seems safe
    shrink_px = 5 * scale 

    if aspect > 1.0:
        print("Detected Pointy Topped Hexagon")
        
        # Calculate R based on both dimensions and take the smaller one
        # to ensuring we don't extend past the content in either direction.
        # Height = 2 * R  => R = Height / 2
        # Width = sqrt(3) * R => R = Width / sqrt(3)
        
        r_h = content_height / 2
        r_w = content_width / math.sqrt(3)
        
        # Use the limiting dimension
        R = min(r_h, r_w) * scale - shrink_px
        
        for i in range(6):
            angle_deg = 30 + 60 * i
            vx = cx_s + R * math.cos(math.radians(angle_deg))
            vy = cy_s + R * math.sin(math.radians(angle_deg)) 
            vertices.append((vx, vy))
            
    else:
        print("Detected Flat Topped Hexagon")
        
        # Height = sqrt(3) * R => R = Height / sqrt(3)
        # Width = 2 * R => R = Width / 2
        
        r_h = content_height / math.sqrt(3)
        r_w = content_width / 2
        
        R = min(r_h, r_w) * scale - shrink_px
        
        for i in range(6):
            angle_deg = 0 + 60 * i
            vx = cx_s + R * math.cos(math.radians(angle_deg))
            vy = cy_s + R * math.sin(math.radians(angle_deg))
            vertices.append((vx, vy))

    # Draw polygon on the mask
    draw.polygon(vertices, fill=255)
    
    # Resize mask down to original size with resampling
    mask_img = mask_img.resize((width, height), resample=Image.LANCZOS)
    
    # Apply Mask
    # Create final image
    # Start with original
    final = img.copy()
    final.putalpha(mask_img)
    
    # Use the mask bbox for the final crop
    final_bbox = mask_img.getbbox() # This works on the resized mask
    if final_bbox:
        final = final.crop(final_bbox)
        
    final.save(output_path)
    print(f"Saved to {output_path}")

if __name__ == "__main__":
    crop_content_to_transparency("logo_rough.png", "logo.png")
