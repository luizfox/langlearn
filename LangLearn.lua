--[[
 Extensão pro VLC que captura as falas não conhecidas  &gt;= 0.1
 Authors: Luiz Felipe
--]]
array = {}
function descriptor()
    return { title = "LangLearn" ;
             version = "0.1" ;
             author = "Luiz" ;
             capabilities = {} }
end

function activate()
    dlg = vlc.dialog("Painel")
    button_play = dlg:add_button("Adicionar - [Tecla 0 (zero)]", click_play, 1, 3, 4, 1)
	button_gravar = dlg:add_button("Gravar", gravar, 1, 4, 4, 1)
	lista = dlg:add_list(1, 5, 4, 1)
	dlg:show()
	
	-- vlc.var.add_callback( vlc.object.vout(), "key-pressed", tecla_apertada, 0)
	vlc.var.add_callback(vlc.object.libvlc(), "key-pressed", key_pressed_handler, 0)

end

function key_pressed_handler( var, old, new, data)
	local key = new
	--vlc.msg.dbg(key)
    if new == 48 then
		click_play()
	end
end

function click_play()
	-- vlc.msg.dbg("Segunda msg Bye bye!")
	local input = vlc.object.input()
	local actual_time = vlc.var.get(input, "time")
	vlc.msg.dbg(actual_time)
	lista:add_value(actual_time, actual_time)
	array[#array + 1] = actual_time
	--vlc.msg.dbg(lista)
end

function gravar()
	file = io.open("J:/dox/lista.txt", "a")
	io.output(file)
	selection = lista:get_selection()
	for i = 1, #array do
        io.write(array[i])
		io.write("\n")
    end
	io.close(file)
end

function deactivate()
  -- if vlc.object.input() and vlc.object.vout() then
    --pcall(vlc.var.del_callback, vlc.object.vout(), "key-pressed", tecla_apertada)
	--pcall(vlc.var.del_callback, vlc.object.libvlc(), "key-pressed", tecla_apertada)
	vlc.var.del_callback(vlc.object.libvlc(), "key-pressed", key_pressed_handler, 0)
  -- end
end

function close()
    vlc.deactivate()
end
