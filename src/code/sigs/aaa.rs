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
