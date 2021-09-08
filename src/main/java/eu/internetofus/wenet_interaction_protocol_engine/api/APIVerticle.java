/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api;

import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngineClient;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.vertx.AbstractAPIVerticle;
import eu.internetofus.common.vertx.AbstractServicesVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.api.events.Events;
import eu.internetofus.wenet_interaction_protocol_engine.api.events.EventsResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.help.Help;
import eu.internetofus.wenet_interaction_protocol_engine.api.help.HelpResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.incentives.Incentives;
import eu.internetofus.wenet_interaction_protocol_engine.api.incentives.IncentivesResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.messages.Messages;
import eu.internetofus.wenet_interaction_protocol_engine.api.messages.MessagesResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.states.States;
import eu.internetofus.wenet_interaction_protocol_engine.api.states.StatesResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.stats.Stats;
import eu.internetofus.wenet_interaction_protocol_engine.api.stats.StatsResource;
import eu.internetofus.wenet_interaction_protocol_engine.api.tasks.Tasks;
import eu.internetofus.wenet_interaction_protocol_engine.api.tasks.TasksResource;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.openapi.RouterBuilder;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The verticle that provide the manage the WeNet interaction protocol engine
 * API.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class APIVerticle extends AbstractAPIVerticle {

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getOpenAPIResourcePath() {

    return "wenet-interaction_protocol_engine-openapi.yaml";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void mountServiceInterfaces(final RouterBuilder routerBuilder) {

    routerBuilder.mountServiceInterface(Help.class, Help.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Help.ADDRESS).register(Help.class, new HelpResource(this));

    routerBuilder.mountServiceInterface(Stats.class, Stats.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Stats.ADDRESS).register(Stats.class, new StatsResource(this.vertx));

    routerBuilder.mountServiceInterface(Messages.class, Messages.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Messages.ADDRESS).register(Messages.class,
        new MessagesResource(this.vertx));

    routerBuilder.mountServiceInterface(Incentives.class, Incentives.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Incentives.ADDRESS).register(Incentives.class,
        new IncentivesResource(this.vertx));

    routerBuilder.mountServiceInterface(Tasks.class, Tasks.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Tasks.ADDRESS).register(Tasks.class, new TasksResource(this.vertx));

    routerBuilder.mountServiceInterface(States.class, States.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(States.ADDRESS).register(States.class, new StatesResource(this.vertx));

    routerBuilder.mountServiceInterface(Events.class, Events.ADDRESS);
    new ServiceBinder(this.vertx).setAddress(Events.ADDRESS).register(Events.class, new EventsResource(this.vertx));

  }

  /**
   * Register the services provided by the API.
   *
   * {@inheritDoc}
   *
   * @see WeNetProfileManager
   */
  @Override
  protected void startedServerAt(final String host, final int port) {

    final var conf = new JsonObject();
    conf.put(WeNetInteractionProtocolEngineClient.INTERACTION_PROTOCOL_ENGINE_CONF_KEY, "http://" + host + ":" + port);
    final var client = AbstractServicesVerticle.createWebClientSession(this.getVertx(), this.config());
    WeNetInteractionProtocolEngine.register(this.vertx, client, conf);

  }

}
