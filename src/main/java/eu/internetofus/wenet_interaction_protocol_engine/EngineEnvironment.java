/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import org.tinylog.Logger;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.interaction_protocol_engine.Message;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;

/**
 * Contains the information of an environment.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class EngineEnvironment {

  /**
   * The profile of the user that has send the message.
   */
  public WeNetUserProfile sender;

  /**
   * The application that has send the message.
   */
  public App app;

  /**
   * The task associated to the message.
   */
  public Task task;

  /**
   * Load a field and set into the environment.
   *
   * @param loader the function to load the necessary field.
   * @param name   of the field.
   * @param type   of the field.
   * @param setter action to set the loaded model into the environment.
   *
   * @param <T>    type of model for the field.
   *
   * @return the future that will provide the loaded field.
   */
  protected static <T extends Model> Function<EngineEnvironment, Future<EngineEnvironment>> loadField(final Class<T> type, final Consumer<Handler<AsyncResult<T>>> loader, final String name, final BiConsumer<EngineEnvironment, T> setter) {

    return env -> {

      final Promise<EngineEnvironment> loadPromise = Promise.promise();
      loader.accept(load -> {
        if (load.failed()) {

          Logger.error(load.cause(), "Can not load the {}", name);

        } else {

          final T result = load.result();
          setter.accept(env, result);
        }
        loadPromise.complete(env);
      });
      return loadPromise.future();
    };
  }

  /**
   * Called when want to load the necessary information into the environment.
   *
   * @param vertx   event bus to use.
   * @param message with the information that has to been loaded.
   *
   * @return the loaded environment.
   */
  public static Future<EngineEnvironment> create(final Vertx vertx, final Message message) {

    final Promise<EngineEnvironment> promise = Promise.promise();
    Future<EngineEnvironment> future = promise.future();

    if (message.senderId != null) {

      future = future.compose(loadField(WeNetUserProfile.class, loader -> {
        WeNetProfileManager.createProxy(vertx).retrieveProfile(message.senderId, loader);
      }, "sender", (env, profile) -> env.sender = profile));

    }
    if (message.appId != null) {

      future = future.compose(loadField(App.class, loader -> {
        WeNetService.createProxy(vertx).retrieveApp(message.appId, loader);
      }, "app", (env, profile) -> env.app = profile));

    }

    if (message.taskId != null) {

      future = future.compose(loadField(Task.class, loader -> {
        WeNetTaskManager.createProxy(vertx).retrieveTask(message.taskId, loader);
      }, "task", (env, profile) -> env.task = profile));

    }

    promise.complete(new EngineEnvironment());
    return future;

  }

}
