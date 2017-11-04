import sys

if (sys.version_info > (3, 0)):
    from okada_wrapper.okada_wrapper import dc3d0wrapper, dc3dwrapper
else:
    from okada_wrapper import dc3d0wrapper, dc3dwrapper
