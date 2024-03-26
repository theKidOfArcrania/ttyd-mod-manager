use super::*;

pub(super) static MAPDELETE_TEMPL: CCodeTemplate<'static> = {
    #[allow(non_upper_case_globals)]
    const wp: usize = 0;
    #[allow(non_upper_case_globals)]
    const fileFree: usize = 1;
    CCodeTemplate {
        name: "mapdelete",
        templ: TemplateRegExp::Fragment(CCodeTemplateFragment {
            constants: &[],
            snippets: &templated!(
                concat!(
                    "    if ({wp} != NULL && {wp}->texture != NULL) {{\n",
                    "        {fileFree}({wp}->texture);\n",
                    "    }}",
                )
            ),
            asm: &insns!(
                stwu r1, [s -16, r1];
                mfspr r0, 8;
                addis r3, r0, [wp@ha];
                stw r0, [s 20, r1];
                addi r3, r3, [wp@lo];
                lwz r3, [s 0, r3];
                cmplwi 0, r3, 0;
                bc 12, 2, 20;
                lwz r3, [s 0, r3];
                cmplwi 0, r3, 0;
                bc 12, 2, 8;
                bl [fileFree@@rel];
                lwz r0, [s 20, r1];
                mtspr 8, r0;
                addi r1, r1, 16;
                bclr 20, 0;
            ),
        }),
        args: &[],
        return_type: "void",
    }
};

pub(super) static MAPDRAW_TEMPL: CCodeTemplate<'static> = {
    #[allow(non_upper_case_globals)]
    const wp: usize = 0;
    #[allow(non_upper_case_globals)]
    const str_mariost_tpl: usize = 1;
    #[allow(non_upper_case_globals)]
    const memset: usize = 2;
    #[allow(non_upper_case_globals)]
    const _mapAlloc: usize = 3;
    #[allow(non_upper_case_globals)]
    const getMarioStDvdRoot: usize = 4;
    #[allow(non_upper_case_globals)]
    const fileAllocf: usize = 5;
    #[allow(non_upper_case_globals)]
    const evtSetValue: usize = 6;
    #[allow(non_upper_case_globals)]
    const float_1000_aaa_000009ac: usize = 7;
    #[allow(non_upper_case_globals)]
    const draw: usize = 8;
    #[allow(non_upper_case_globals)]
    const dispEntry: usize = 9;
    #[allow(non_upper_case_globals)]
    const mapalloc_base_ptr: usize = 10;
    CCodeTemplate {
        name: "mapdraw",
        templ: TemplateRegExp::Fragment(CCodeTemplateFragment {
            constants: &[float_1000_aaa_000009ac],
            snippets: &templated!(
                concat!(
                    "    if (isFirstCall) {{\n",
                    "        {wp} = {_mapAlloc}({mapalloc_base_ptr}, 8);\n",
                    "        memset({wp}, 0, sizeof(MarioHouseWork));\n",
                    "        {wp}->texture = {fileAllocf}(4, {str_mariost_tpl}, ",
                            "{getMarioStDvdRoot}());\n",
                    "        {wp}->alpha = 0;\n",
                    "    }}\n",
                    "    {wp}->alpha += 2;\n",
                    "    if ({wp}->alpha > 255) {{\n",
                    "        {wp}->alpha = 255;\n",
                    "        {evtSetValue}(event, GF(0), 1); //done rolling in\n",
                    "    }}\n",
                    "    {dispEntry}(CAMERA_3D, 7, draw, 1000.0f, {wp});\n",
                    "    return EVT_RETURN_BLOCK;",
                )
            ),
            asm: &insns!(
                stwu r1, [s -16, r1];
                mfspr r0, 8;
                cmpwi 0, r4, 0;
                stw r0, [s 20, r1];
                stw r31, [s 12, r1];
                or r31, r3, r3;
                bc 12, 2, 104;
                addis r3, r0, [mapalloc_base_ptr@ha];
                addi r4, r0, 8;
                addi r3, r3, [mapalloc_base_ptr@lo];
                lwz r3, [s 0, r3];
                bl [_mapAlloc@@rel];
                addis r5, r0, [wp@ha];
                addi r4, r0, 0;
                addi r6, r5, [wp@lo];
                addi r5, r0, 8;
                stw r3, [s 0, r6];
                bl [memset@@rel];
                bl [getMarioStDvdRoot@@rel];
                addis r4, r0, [str_mariost_tpl@ha];
                or r5, r3, r3;
                addi r4, r4, [str_mariost_tpl@lo];
                addi r3, r0, 4;
                crxor 6, 6, 6;
                bl [fileAllocf@@rel];
                addis r4, r0, [wp@ha];
                addi r0, r0, 0;
                addi r5, r4, [wp@lo];
                lwz r4, [s 0, r5];
                stw r3, [s 0, r4];
                lwz r3, [s 0, r5];
                stw r0, [s 4, r3];
                addis r3, r0, [wp@ha];
                addi r5, r3, [wp@lo];
                lwz r4, [s 0, r5];
                lwz r3, [s 4, r4];
                addi r0, r3, 2;
                stw r0, [s 4, r4];
                lwz r3, [s 0, r5];
                lwz r0, [s 4, r3];
                cmpwi 0, r0, 255;
                bc 4, 1, 32;
                addi r0, r0, 255;
                addis r4, r0, [s -1373];
                stw r0, [s 4, r3];
                or r3, r31, r31;
                addi r4, r4, [s -19072];
                addi r5, r0, 1;
                bl [evtSetValue@@rel];
                addis r4, r0, [float_1000_aaa_000009ac@ha];
                addis r3, r0, [wp@ha];
                addi r5, r4, [float_1000_aaa_000009ac@lo];
                lwz r6, [wp@lo, r3];
                addis r4, r0, [draw@ha];
                lfs r1, [s 0, r5];
                addi r5, r4, [draw@lo];
                addi r3, r0, 4;
                addi r4, r0, 7;
                bl [dispEntry@@rel];
                lwz r0, [s 20, r1];
                addi r3, r0, 0;
                lwz r31, [s 12, r1];
                mtspr 8, r0;
                addi r1, r1, 16;
                bclr 20, 0;
            ),
        }),
        args: &["EventEntry* event", "bool isFirstCall"],
        return_type: "int",
    }
};

pub(super) static DRAW_TEMPL: CCodeTemplate<'static> = {
    #[allow(non_upper_case_globals)]
    const GXGetViewportv: usize = 0;
    #[allow(non_upper_case_globals)]
    const GXSetViewport: usize = 1;
    #[allow(non_upper_case_globals)]
    const GXGetProjectionv: usize = 2;
    #[allow(non_upper_case_globals)]
    const GXSetProjection: usize = 3;
    #[allow(non_upper_case_globals)]
    const GXSetProjectionv: usize = 4;
    #[allow(non_upper_case_globals)]
    const camGetPtr: usize = 5;
    #[allow(non_upper_case_globals)]
    const camGetCurPtr: usize = 6;
    #[allow(non_upper_case_globals)]
    const winTexSet: usize = 7;
    #[allow(non_upper_case_globals)]
    const winTexInit: usize = 8;
    #[allow(non_upper_case_globals)]
    const wp: usize = 9;
    #[allow(non_upper_case_globals)]
    const dat_aaa_000005e8: usize = 10;
    #[allow(non_upper_case_globals)]
    const vec3_aaa_000005d0: usize = 11;
    #[allow(non_upper_case_globals)]
    const vec3_aaa_000005dc: usize = 12;

    CCodeTemplate {
        name: "draw",
        templ: TemplateRegExp::Fragment(CCodeTemplateFragment {
            constants: &[dat_aaa_000005e8],
            snippets: &templated!(
                concat!(
                    "    f32 old_viewport[GX_VIEWPORT_SZ];\n",
                    "    f32 old_projection[GX_PROJECTION_SZ];\n",
                    "    CameraEntry old_camera;\n",
                    "    Vec translate, scale;\n",
                    "    GXColor color;\n",
                    "\n",
                    "    // Preserve old projection/views\n",
                    "    {GXGetViewportv}(old_viewport);\n",
                    "    {GXGetProjectionv}(old_projection);\n",
                    "\n",
                    "    // Draw 2d texture onto screen\n",
                    "    {GXSetProjection}({camGetPtr}(CAMERA_2D)->projection, {camGetPtr}(CAMERA_2D)->type);\n",
                    "    old_camera = *{camGetCurPtr}();\n",
                    "    *{camGetCurPtr}() = *{camGetPtr}(CAMERA_2D);\n",
                    "    {winTexInit}(*wp->texture->data);\n",
                    "    color = (GXColor){{0xFF, 0xFF, 0xFF, (u8)wp->alpha}};\n",
                    "    scale = {vec3_aaa_000005dc};\n", // TODO: if we inline constants here,
                                                          // weird stuff happens...
                    "    translate = {vec3_aaa_000005d0};\n",
                    "    {winTexSet}(3, translate, scale, color); // render the map image\n",
                    "\n",
                    "    // Restore old settings\n",
                    "    *{camGetCurPtr}() = old_camera;\n",
                    "    {GXSetViewport}(old_viewport[0], old_viewport[1], old_viewport[2],\n",
                    "         old_viewport[3], old_viewport[4], old_viewport[5]);\n",
                    "    {GXSetProjectionv}(old_projection);",
                )
            ),
            asm: &insns!(
                stwu r1, [s -720, r1];
                mfspr r0, 8;
                stw r0, [s 724, r1];
                addi r3, r1, 68;
                stw r31, [s 716, r1];
                bl [GXGetViewportv@@rel];
                addi r3, r1, 40;
                bl [GXGetProjectionv@@rel];
                addi r3, r0, 8;
                bl [camGetPtr@@rel];
                or r31, r3, r3;
                addi r3, r0, 8;
                bl [camGetPtr@@rel];
                lwz r4, [s 412, r31];
                addi r3, r3, 348;
                bl [GXSetProjection@@rel];
                bl [camGetCurPtr@@rel];
                addi r0, r0, 76;
                addi r5, r1, 92;
                addi r4, r3, [s -4];
                mtspr 9, r0;
                lwz r3, [s 4, r4];
                lwzu r0, [s 8, r4];
                stw r3, [s 4, r5];
                stwu r0, [s 8, r5];
                bc 16, 0, [s -16];
                addi r3, r0, 8;
                bl [camGetPtr@@rel];
                or r31, r3, r3;
                bl [camGetCurPtr@@rel];
                addi r0, r0, 76;
                addi r5, r3, [s -4];
                addi r4, r31, [s -4];
                mtspr 9, r0;
                lwz r3, [s 4, r4];
                lwzu r0, [s 8, r4];
                stw r3, [s 4, r5];
                stwu r0, [s 8, r5];
                bc 16, 0, [s -16];
                addis r3, r0, [wp@ha];
                addi r3, r3, [wp@lo];
                lwz r3, [s 0, r3];
                lwz r3, [s 0, r3];
                lwz r3, [s 160, r3];
                lwz r3, [s 0, r3];
                bl [winTexInit@@rel];
                addis r3, r0, [dat_aaa_000005e8@ha];
                addis r4, r0, [wp@ha];
                addi r5, r3, [dat_aaa_000005e8@lo];
                addi r6, r1, 12;
                lwz r0, [s 0, r5];
                addi r4, r4, [wp@lo];
                lwz r4, [s 0, r4];
                addis r3, r0, [vec3_aaa_000005dc@ha];
                stw r0, [s 8, r1];
                addi r7, r3, [vec3_aaa_000005dc@lo];
                lwz r0, [s 4, r4];
                addis r3, r0, [vec3_aaa_000005d0@ha];
                addi r9, r3, [vec3_aaa_000005d0@lo];
                lwz r12, [s 0, r7];
                stb r0, [s 11, r1];
                addi r4, r1, 28;
                lwz r11, [s 4, r7];
                addi r5, r1, 16;
                lwz r10, [s 8, r7];
                addi r3, r0, 3;
                lwz r31, [s 8, r1];
                lwz r8, [s 0, r9];
                lwz r7, [s 4, r9];
                lwz r0, [s 8, r9];
                stw r31, [s 12, r1];
                stw r12, [s 16, r1];
                stw r11, [s 20, r1];
                stw r10, [s 24, r1];
                stw r8, [s 28, r1];
                stw r7, [s 32, r1];
                stw r0, [s 36, r1];
                bl [winTexSet@@rel];
                bl [camGetCurPtr@@rel];
                addi r0, r0, 76;
                addi r5, r3, [s -4];
                addi r4, r1, 92;
                mtspr 9, r0;
                lwz r3, [s 4, r4];
                lwzu r0, [s 8, r4];
                stw r3, [s 4, r5];
                stwu r0, [s 8, r5];
                bc 16, 0, [s -16];
                lfs r1, [s 68, r1];
                lfs r2, [s 72, r1];
                lfs r3, [s 76, r1];
                lfs r4, [s 80, r1];
                lfs r5, [s 84, r1];
                lfs r6, [s 88, r1];
                bl [GXSetViewport@@rel];
                addi r3, r1, 40;
                bl [GXSetProjectionv@@rel];
                lwz r0, [s 724, r1];
                lwz r31, [s 716, r1];
                mtspr 8, r0;
                addi r1, r1, 720;
                bclr 20, 0;
            ),
        }),
        args: &["CameraId cameraId", "void* param"],
        return_type: "void",
    }
};
