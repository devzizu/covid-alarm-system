package diretorio;

import diretorio.health.TemplateHealthCheck;
import diretorio.resources.*;
import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class DiretorioApplication extends Application<DiretorioConfiguration> {

    public static void main(final String[] args) throws Exception {
        new DiretorioApplication().run(args);
    }

    @Override
    public String getName() {
        return "Diretorio";
    }

    @Override
    public void initialize(final Bootstrap<DiretorioConfiguration> bootstrap) {
    }

    @Override
    public void run(final DiretorioConfiguration configuration,
                    final Environment environment) {
        environment.jersey().register(
            new DiretorioResource(configuration.template,configuration.districts));
        environment.healthChecks().register("template",
            new TemplateHealthCheck(configuration.template));
        
    }

}
